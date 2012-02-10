(defpackage #:conserv-echo
  (:use :cl :conserv))
(in-package #:conserv-echo)

;; Any class can be used as a driver. All event implementations are optional.
(defclass echo () ())

(defvar *server*)
(defmethod on-listen ((driver echo))
  (format t "~&Server up! Listening on ~A:~A~%"
          (server-name *server*)
          (server-port *server*)))

;; ON-CONNECT is called whenever a new client is connected to the server.
(defmethod on-connect ((driver echo) client)
  (format t "~&Client connected. Host: ~A, Port: ~A~%"
          (client-remote-name client) (client-remote-port client))
  ;; Clients are character output streams, so FORMAT/PRINC/ETC can all be used.
  (format client "~&Welcome. Everything you say will be echoed back to you now.~%"))

;; ON-DATA is called whenever data has been received from the client.
(defmethod on-data ((driver echo) client data)
  (format t "~&Data length: ~S~%" (length data))
  ;; Just write the sequence back into the client.
  (write-sequence data client))

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
                             :binaryp nil)))
    (server-listen *server*)))
