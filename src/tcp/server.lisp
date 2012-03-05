(in-package #:conserv.tcp)

(defvar *server*)
(setf (documentation '*server* 'variable)
      "During execution of `server-event-driver` events, this variable is bound to the associated
      `server` object. This variable is unbound outside of the scope of `server-event-driver`
      events.")
(defprotocol server-event-driver (a)
  ((listen ((driver a))
    :default-form nil
    :documentation "Event called when `*server*` has started listening.")
   (connection ((driver a) client)
    :default-form nil
    :documentation "Event called when a new `client` (a `socket`) has successfully connected to `*server*`.")
   (error ((driver a) error)
    :default-form (invoke-debugger error)
    :documentation "Event called when `*server*` has experienced some error. `error` is the actual
                    condition that was signaled. This event is called immediately before the server
                    and all its connected client sockets are closed.")
   (close ((driver a))
    :default-form nil
    :documentation "Event called after the server has been closed."))
  (:prefix on-server-)
  (:documentation "Protocol for TCP servers."))

(defprotocol server (a)
  ((external-format-in ((server a))
    :accessorp t
    :documentation "Default external-format-in for incoming connections.")
   (external-format-out ((server a))
    :accessorp t
    :documentation "Default external-format-out for incoming connections.")
   (driver ((server a))
    :documentation "Driver object used to dispatch `server-event-driver` events on `server`.")
   (socket ((server a)) :accessorp t
    :documentation "The conserv socket associated with this server.")
   (client-driver ((server a))
    :documentation "Driver to use when accepting new client sockets.")))

(defclass server ()
  ((driver :initarg :driver :reader server-driver)
   (client-driver :initarg :client-driver :reader server-client-driver)
   (external-format-in :initarg :external-format-in :accessor server-external-format-in)
   (external-format-out :initarg :external-format-out :accessor server-external-format-out)
   (connections :accessor server-connections :initform (make-hash-table))
   (socket :initarg :socket :accessor server-socket)))

(defun make-server (driver
                    &key
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*)
                    (client-driver driver))
  (make-instance 'server
                 :driver driver
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :client-driver client-driver))

(defun server-list-clients (server)
  "Returns a list of all `socket` objects connected to `server`."
  (hash-table-values (server-connections server)))
(defun server-count-clients (server)
  "Returns the number of `socket` objects connected to `server`."
  (hash-table-count (server-connections server)))

(defun server-event-base (server)
  (declare (ignore server))
  (or *event-base*
      (error "Operation not supported outside of the scope of WITH-EVENT-LOOP.")))

(defmethod close ((server server) &key abort)
  (mapcar (rcurry #'close :abort abort) (server-list-clients server))
  (clrhash (server-connections server))
  (when (socket-local-p (server-socket server))
    (delete-file (pathname (socket-local-name (server-socket server)))))
  (close (server-socket server) :abort abort)
  (unregister-socket server)
  (let ((*server* server))
    (on-server-close (server-driver server))))

(defun server-pause (server &key timeout)
  "Pauses read events for `server`, preventing it from accepting new connections. While paused, all
   `socket`s already connected to `server` continue to work normally, but new connections will not
   be accepted. Incoming clients are not immediately turned away, although they may time out while
   connecting if the server is paused for too long. If `timeout` is provided, it is interpreted as
   the number of seconds to wait before starting to accept clients again. This function has no
   effect if `server` is already paused, although the resume timeout will still be activated."
  (socket-pause (server-socket server))
  (when timeout
    (add-timer (curry #'server-resume server) timeout :one-shot t)))

(defun server-resume (server)
  "Resumes read events for `server`, enabling new client connections. If `server` was already
   accepting clients, this function has no effect."
  (when (socket-paused-p (server-socket server))
    (iolib:set-io-handler
     (server-event-base server)
     (iolib:socket-os-fd (socket-internal-socket (server-socket server)))
     :read (lambda (&rest ig)
             (declare (ignore ig))
             (when-let (client-sock (iolib:accept-connection (socket-internal-socket
                                                              (server-socket server))))
               (let ((socket (make-socket (server-client-driver server)
                                          :external-format-in (server-external-format-in server)
                                          :external-format-out (server-external-format-out server))))
                 (setf (gethash socket (server-connections server)) socket
                       (socket-internal-socket socket) client-sock
                       (socket-server socket) server)
                 (let ((*server* server))
                   (on-server-connection (server-driver server) socket))
                 (socket-resume socket)
                 (start-writes socket)))))))

(defun server-listen (driver &key
                      (host iolib:+ipv4-unspecified+)
                      (port (unless (pathnamep host) 1337))
                      (external-format-in *default-external-format*)
                      (external-format-out *default-external-format*)
                      (client-driver driver))
  "Starts a TCP listener. If successful, returns a `server` object.

   * `driver` -- An instance of a driver class that will be used to dispatch `server-event-driver` events. This driver will also be set as the `socket-driver` for incoming connections.
   * `host` -- Either a string representing a local IP address to bind to, or a pathname to use as a unix socket.
   * `port` -- Port to listen on. Should not be provided if `host` is a unix socket.
   * `external-format-in` -- Default external-format-in for client connections.
   * `external-format-out` -- Default external-format-out for client connections."
  (let* ((server (make-server driver
                              :external-format-in external-format-in
                              :external-format-out external-format-out
                              :client-driver client-driver))
         (socket-args `(:connect :passive
                                 :address-family ,(if (pathnamep host)
                                                      :local
                                                      :internet)
                                 :backlog 5
                                 :ipv6 nil
                                 ,@(if (pathnamep host)
                                       `(:local-filename ,(namestring host))
                                       `(:local-host ,host))
                                 ,@(unless (pathnamep host)
                                           `(:local-port ,port))))
         (internal-socket (apply #'iolib:make-socket socket-args))
         (socket (make-socket (server-driver server))))
    (setf (socket-internal-socket socket) internal-socket
          (server-socket server) socket)
    (register-socket server)
    (let ((*server* server))
      (on-server-listen (server-driver server)))
    (server-resume server)
    server))
