{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Socket.BufferPool.Recv (
    receive,
    receiveNoWait,
    receiveWith,
    makeRecvN,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString (..), unsafeCreate)
import Data.IORef
import Network.Socket (Socket, recvBuf, recvBufNoWait)
#if defined(mingw32_HOST_OS)
import Control.Concurrent (forkIO)
import Control.Monad (void)
#else
import Data.Functor (($>))
import Network.Socket (waitAndCancelReadSocketSTM)
#endif

import Network.Socket.BufferPool.Buffer
import Network.Socket.BufferPool.Types

----------------------------------------------------------------

-- | The receiving function with a buffer pool.
--   The buffer pool is automatically managed.
--
-- /Caveats of 'BufferPool' apply./
receive :: Socket -> BufferPool -> Recv
receive sock pool = withBufferPool pool $ \ptr size -> recvBuf sock ptr size

-- | Like 'receive' but never blocks and never involves the IO manager:
--   'Nothing' means no data was available (or an error occurred, which a
--   subsequent blocking 'receive' will report properly). @Just \"\"@ is EOF.
--
-- /Caveats of 'BufferPool' apply./
receiveNoWait :: Socket -> BufferPool -> IO (Maybe ByteString)
receiveNoWait sock pool = tryWithBufferPool pool $ \ptr size ->
    -- Both EAGAIN and real errors map to a negative result, deferring
    -- to the blocking path so errors surface there with their errno
    -- intact.
    fromIntegral <$> recvBufNoWait sock ptr size

-- | Like 'receive', but giving up when the given STM action succeeds
--   rather than waiting for the peer for ever.  @Left@ is what that
--   action returned, @Right@ is what was received, with @Right \"\"@ for
--   EOF as in 'receive'.  The action is looked at before anything is
--   read, so nothing is read once it succeeds.
--
--   The wait on the socket is cancelled however this ends: an IO manager
--   built on an interface like @io_uring@ holds a reference on the socket
--   for as long as the poll it was asked for is outstanding, so a wait
--   left behind by a caller which gave up is a socket which does not
--   close.
--
--   On Windows a read cannot be waited on, so it is run on a thread of
--   its own and what it read is handed back.  A read this caller has
--   stopped waiting for fills a buffer that is nobody's by then.
--
-- /Caveats of 'BufferPool' apply./
receiveWith :: Socket -> BufferPool -> STM a -> IO (Either a ByteString)
receiveWith sock pool giveUp = do
    -- Given up already: nothing is read, even if it is there.
    early <- atomically $ (Just <$> giveUp) <|> return Nothing
    case early of
        Just a -> return $ Left a
        Nothing -> do
            -- Nothing to wait for if it is already there.
            mbs <- receiveNoWait sock pool
            case mbs of
                Just bs -> return $ Right bs
                Nothing -> slow
  where
    recv = receive sock pool
#if !defined(mingw32_HOST_OS)
    slow = do
        (readable, cancelWait) <- waitAndCancelReadSocketSTM sock
        ev <- flip E.finally cancelWait $ atomically $
            (Left <$> giveUp) <|> (readable $> Right ())
        either (return . Left) (const $ Right <$> recv) ev
#else
    slow = do
        var <- newEmptyTMVarIO
        void $ forkIO $ do
            r <- E.try recv
            atomically $ putTMVar var (r :: Either E.SomeException ByteString)
        ev <- atomically $
            (Left <$> giveUp)
                <|> (Right <$> (takeTMVar var >>= either throwSTM return))
        return ev
#endif

----------------------------------------------------------------

-- | This function returns a receiving function
--   based on two receiving functions.
--   The returned function receives exactly N bytes.
--   The first argument is an initial received data.
--   After consuming the initial data, the two functions is used.
--   When N is less than equal to 4096, the buffer pool is used.
--   Otherwise, a new buffer is allocated.
--   In this case, the global lock is taken.
--
-- >>> :seti -XOverloadedStrings
-- >>> tryRecvN "a" 3 =<< _iorefRecv ["bcd"]
-- ("abc","d")
-- >>> tryRecvN "a" 3 =<< _iorefRecv ["bc"]
-- ("abc","")
-- >>> tryRecvN "a" 3 =<< _iorefRecv ["b"]
-- ("ab","")
--
-- /The resulting 'RecvN' is designed to be used in one thread only./
makeRecvN :: ByteString -> Recv -> IO RecvN
makeRecvN bs0 recv = do
    ref <- newIORef bs0
    return $ recvN ref recv

-- | The receiving function which receives exactly N bytes
--   (the fourth argument).
recvN :: IORef ByteString -> Recv -> RecvN
recvN ref recv size = do
    cached <- readIORef ref
    (bs, leftover) <- tryRecvN cached size recv
    writeIORef ref leftover
    return bs

----------------------------------------------------------------

tryRecvN :: ByteString -> Int -> IO ByteString -> IO (ByteString, ByteString)
tryRecvN init0 siz0 recv
    | siz0 <= len0 = return $ BS.splitAt siz0 init0
    | otherwise = go (init0 :) (siz0 - len0)
  where
    len0 = BS.length init0
    go build left = do
        bs <- recv
        let len = BS.length bs
        if len == 0
            then do
                let cs = concatN (siz0 - left) $ build []
                return (cs, "")
            else
                if len >= left
                    then do
                        let (consume, leftover) = BS.splitAt left bs
                            ret = concatN siz0 $ build [consume]
                        return (ret, leftover)
                    else do
                        let build' = build . (bs :)
                            left' = left - len
                        go build' left'

concatN :: Int -> [ByteString] -> ByteString
-- Just because it's logical
concatN _ [] = ""
-- To avoid a copy if there's only one ByteString
concatN _ [bs] = bs
concatN total bss0 =
    unsafeCreate total $ \ptr -> goCopy bss0 ptr
  where
    goCopy [] _ = return ()
    goCopy (bs : bss) ptr = do
        ptr' <- copy ptr bs
        goCopy bss ptr'

-- | doctest only. Elements in the argument must not be empty.
_iorefRecv :: [ByteString] -> IO (IO ByteString)
_iorefRecv ini = do
    ref <- newIORef ini
    return $ recv ref
  where
    recv ref = do
        xxs <- readIORef ref
        case xxs of
            [] -> do
                writeIORef ref $ error "closed"
                return ""
            x : xs -> do
                writeIORef ref xs
                return x
