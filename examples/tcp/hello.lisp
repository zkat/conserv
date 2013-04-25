(defpackage #:conserv.examples.tcp.hello
  (:use #:cl)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.tcp
                #:tcp-listen
                #:on-tcp-listener-connection))
(in-package #:conserv.examples.tcp.hello)

(defclass hello () ())

(defmethod on-tcp-listener-connection ((driver hello) tcp-client)
  (format tcp-client "~&Hello, world!~%")
  (close tcp-client))

;; Call as (start t) to use local IPC sockets instead of binding to a host/port.
(defun start (&optional localp)
  (with-event-loop ()
    (tcp-listen (make-instance 'hello)
                :host (if localp #p"/tmp/conserv-example.sock" "127.0.0.1")
                :port (unless localp 1337))))
