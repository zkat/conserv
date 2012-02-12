(in-package #:conserv)

(defprotocol tcp-server-event-driver ()
  ((listen (driver server) ; TODO - pass the listening address and port?
    :default-form nil
    :documentation "Event called when SERVER has started listening.")
   (connection (driver server client)
    :default-form nil
    :documentation "Event called when a new CLIENT has successfully connected to SERVER.")
   (error (driver server error)
    :default-form (invoke-debugger error)
    :documentation "Event called when the server has experienced some error. ERROR is the actual
                    condition that was signaled. This event is called immediately before the server
                    and all its connected client sockets are closed.")
   (close (driver server)
    :default-form nil
    :documentation "Event called when the server has been closed."))
  (:prefix on-tcp-server-)
  (:documentation "Protocol for TCP servers."))

(defprotocol server (fundamental-stream)
  ((external-format-in :reader)
   (external-format-out :reader)
   (driver :reader)
   (binary-p :accessor)
   (socket :accessor
    :documentation "The conserv socket associated with this server.")
   (client-driver :reader
    :documentation "Driver to use when accepting new client sockets.")))

(defclass server ()
  ((driver :initarg :driver :reader server-driver)
   (client-driver :initarg :client-driver :reader server-client-driver)
   (external-format-in :initarg :external-format-in :reader server-external-format-in)
   (external-format-out :initarg :external-format-out :reader server-external-format-out)
   (binaryp :initarg :binaryp :reader server-binary-p)
   (connections :accessor server-connections :initform (make-hash-table))
   (socket :initarg :socket :accessor server-socket)))

(defun make-server (driver
                    &key
                    binaryp
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*)
                    (client-driver driver))
  (make-instance 'server
                 :driver driver
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :binaryp binaryp
                 :client-driver client-driver))

(defun server-list-clients (server)
  (hash-table-values (server-connections server)))
(defun server-count-clients (server)
  (hash-table-count (server-connections server)))

(defun server-event-base (server)
  (declare (ignore server))
  (or *event-base*
      (error "Operation not supported outside of the scope of WITH-EVENT-LOOP.")))

(defmethod close ((server server) &key abort)
  (mapcar (rcurry #'close :abort abort) (server-list-clients server))
  (clrhash (server-connections server))
  (close (server-socket server) :abort abort)
  (unregister-socket server)
  (on-tcp-server-close (server-driver server) server))

(defun server-pause (server &key timeout)
  (socket-pause (server-socket server))
  (when timeout
    (add-timer (curry #'server-resume server) timeout :one-shot t)))

(defun server-resume (server)
  (iolib:set-io-handler
   (server-event-base server)
   (iolib:socket-os-fd (socket-internal-socket (server-socket server)))
   :read (lambda (&rest ig)
           (declare (ignore ig))
           (when-let (client-sock (iolib:accept-connection (socket-internal-socket
                                                            (server-socket server))))
             (let ((socket (make-socket (server-client-driver server)
                                        :external-format-in (server-external-format-in server)
                                        :external-format-out (server-external-format-out server)
                                        :binaryp (server-binary-p server))))
               (setf (gethash socket (server-connections server)) socket
                     (socket-internal-socket socket) client-sock
                     (socket-server socket) server)
               (on-tcp-server-connection (server-driver server) server socket)
               (socket-resume socket)
               (start-writes socket))))))

(defun server-listen (server &key (host iolib:+ipv4-unspecified+) (port 1337))
  "Binds the server's socket to an address and starts listening on it."
  (let ((internal-socket (iolib:make-socket :connect :passive
                                            :ipv6 nil
                                            :backlog 5
                                            :local-host host
                                            :local-port port
                                            :reuse-address t))
        (socket (make-socket (server-driver server))))
    (setf (socket-internal-socket socket) internal-socket
          (server-socket server) socket)
    (register-socket server)
    (on-tcp-server-listen (server-driver server) server)
    (server-resume server)
    server))
