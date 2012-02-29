(defpackage #:conserv.examples.tcp.hello
  (:use #:cl)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.tcp
                #:server-listen
                #:on-server-connection))
(in-package #:conserv.examples.tcp.hello)

(defclass hello () ())

(defmethod on-server-connection ((driver hello) socket)
  (write-sequence "Hello, world!" socket)
  (close socket))

;; Call as (start t) to use local IPC sockets instead of binding to a host/port.
(defun start (&optional localp)
  (with-event-loop ()
    (server-listen (make-instance 'hello)
                   :host (if localp #p"/tmp/conserv-example.sock" "127.0.0.1")
                   :port (unless localp 1337))))
