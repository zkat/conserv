(defpackage #:conserv.examples.tcp.echo
  (:use #:cl)
  (:import-from #:conserv
                #:with-event-loop)
  (:import-from #:conserv.tcp
                #:*socket*
                #:server-listen
                #:on-socket-data))
(in-package #:conserv.examples.tcp.echo)

(defclass echo () ())

(defmethod on-socket-data ((driver echo) data)
  (write-sequence data *socket*))

(defun start ()
  (with-event-loop ()
    (server-listen (make-instance 'echo)
                   :host "127.0.0.1"
                   :port 1337)))
