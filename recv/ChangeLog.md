# ChangeLog for "recv"

## v0.1.3

* Providing `receiveWith`: `receive` which gives up when a given STM
  action succeeds, rather than waiting for the peer for ever.  The wait
  on the socket is cancelled however it ends.
  [#1122](https://github.com/yesodweb/wai/pull/1122)

## v0.1.2

* Providing `receiveNoWait` and `tryWithBufferPool`.
  [#1107](https://github.com/yesodweb/wai/pull/1107)

## v0.1.1

* Fixing the bug that the last chunk is skipped when the size is
  insufficient [#1031](https://github.com/yesodweb/wai/pull/1031)
