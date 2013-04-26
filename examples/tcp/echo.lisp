(defpackage #:conserv.examples.tcp.echo
  (:use #:cl)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.tcp
                #:*tcp-client*
                #:tcp-listen
                #:on-tcp-client-data))
(in-package #:conserv.examples.tcp.echo)

(defclass echo () ())

(defmethod on-tcp-client-data ((driver echo) data)
  (write-sequence data *tcp-client*))

(defun start ()
  (with-event-loop ()
    (tcp-listen (make-instance 'echo)
                :host "127.0.0.1"
                :port 1337)))
