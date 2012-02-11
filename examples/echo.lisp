(defpackage #:conserv-echo
  (:use :cl :conserv))
(in-package #:conserv-echo)

;; "Bare minimum" for an echo server, with only a couple of extra things.
(defvar *server*)
(defclass echo () ())
(defun start ()
  (let ((*server* (make-server (make-instance 'echo)
                               ;; No point in encoding if we're just passing the data through.
                               :binaryp t)))
    (server-listen *server*)))
(defmethod on-client-data ((driver echo) client data)
  (write-sequence data client) ; Write it right back.
  (format t "~&Data length: ~S, Bytes written: ~S, Bytes read: ~S~%"
          (length data)
          (client-bytes-written client)
          (client-bytes-read client)))


;; Other goodies to show off
(defmethod on-server-listen ((driver echo))
  (format t "~&Server up! Listening on ~A:~A~%"
          (server-name *server*)
          (server-port *server*)))

(defmethod on-client-connect ((driver echo) client)
  (format t "~&Client connected. Host: ~A, Port: ~A~%"
          (client-remote-name client) (client-remote-port client))
  ;; Clients are character output streams, so FORMAT/PRINC/ETC can all be used.
  (format client "~&Welcome. Everything you say will be echoed back to you now.~%"))

(defmethod on-client-close ((driver echo) client)
  (format t "~&Client disconnected: ~S~%" client))
