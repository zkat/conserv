(defpackage #:conserv-echo
  (:use :cl :conserv))
(in-package #:conserv-echo)

;; Any class can be used as a driver. All event implementations are optional.
(defclass echo () ())

(defvar *server*)
(defmethod on-server-listen ((driver echo))
  (format t "~&Server up! Listening on ~A:~A~%"
          (server-name *server*)
          (server-port *server*)))

(defmethod on-client-connect ((driver echo) client)
  (format t "~&Client connected. Host: ~A, Port: ~A~%"
          (client-remote-name client) (client-remote-port client))
  ;; Clients are character output streams, so FORMAT/PRINC/ETC can all be used.
  (format client "~&Welcome. Everything you say will be echoed back to you now.~%"))

(defmethod on-client-data ((driver echo) client data)
  ;; Just write the sequence back into the client.
  (write-sequence data client)
  (format t "~&Data length: ~S, Bytes written: ~S, Bytes read: ~S~%"
          (length data)
          (client-bytes-written client)
          (client-bytes-read client)))

(defmethod on-client-close ((driver echo) client)
  (format t "~&Client disconnected: ~S~%" client))

(defun start ()
  (let ((*server* (make-server (make-instance 'echo)
                             ;; We configure the server to be binary, since we'll only pipe
                             ;; information back to clients as it comes in. Without this option,
                             ;; :external-format-in is used to encode data received from clients.
                             ;;
                             ;; Note that regardless of this option, clients can still be written to
                             ;; as character streams. :external-format-out will be used to encode
                             ;; the outgoing character data.
                             :binaryp t)))
    (server-listen *server*)))
