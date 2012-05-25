(in-package #:conserv.tcp)

(defvar *tcp-listener*)
(setf (documentation '*tcp-listener* 'variable)
      "During execution of `tcp-listener-event-driver` events, this variable is bound to the associated
      `tcp-listener` object. This variable is unbound outside of the scope of `tcp-listener-event-driver`
      events.")
(defprotocol tcp-listener-event-driver (a)
  ((listen ((driver a))
    :default-form nil
    :documentation "Event called when `*tcp-listener*` has started listening.")
   (connection ((driver a) client)
    :default-form nil
    :documentation "Event called when a new `client` (a `tcp-client`) has successfully connected to `*tcp-listener*`.")
   (error ((driver a) error)
    :default-form (invoke-debugger error)
    :documentation "Event called when `*tcp-listener*` has experienced some error. `error` is the actual
                    condition that was signaled. This event is called immediately before the tcp-listener
                    and all its connected client tcp-clients are closed.")
   (close ((driver a))
    :default-form nil
    :documentation "Event called after the tcp-listener has been closed."))
  (:prefix on-tcp-listener-)
  (:documentation "Protocol for TCP tcp-listeners."))

(defprotocol tcp-listener (a)
  ((external-format-in ((tcp-listener a))
    :accessorp t
    :documentation "Default external-format-in for incoming connections.")
   (external-format-out ((tcp-listener a))
    :accessorp t
    :documentation "Default external-format-out for incoming connections.")
   (driver ((tcp-listener a))
    :documentation "Driver object used to dispatch `tcp-listener-event-driver` events on `tcp-listener`.")
   (tcp-client ((tcp-listener a)) :accessorp t
    :documentation "The conserv tcp-client associated with this tcp-listener.")
   (client-driver ((tcp-listener a))
    :documentation "Driver to use when accepting new tcp-clients.")))

(defclass tcp-listener ()
  ((driver :initarg :driver :reader tcp-listener-driver)
   (client-driver :initarg :client-driver :reader tcp-listener-client-driver)
   (external-format-in :initarg :external-format-in :accessor tcp-listener-external-format-in)
   (external-format-out :initarg :external-format-out :accessor tcp-listener-external-format-out)
   (connections :accessor tcp-listener-connections :initform (make-hash-table))
   (tcp-client :initarg :tcp-client :accessor tcp-listener-tcp-client)))

(defun make-tcp-listener (driver
                    &key
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*)
                    (client-driver driver))
  (make-instance 'tcp-listener
                 :driver driver
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :client-driver client-driver))

(defun tcp-listener-event-base (listener)
  (declare (ignore listener))
  (or *event-base*
      (error "Operation not supported outside of the scope of `with-event-loop`")))

(defun tcp-listener-list-clients (tcp-listener)
  "Returns a list of all `tcp-client` objects connected to `tcp-listener`."
  (hash-table-values (tcp-listener-connections tcp-listener)))
(defun tcp-listener-count-clients (tcp-listener)
  "Returns the number of `tcp-client` objects connected to `tcp-listener`."
  (hash-table-count (tcp-listener-connections tcp-listener)))

(defmethod close ((tcp-listener tcp-listener) &key abort)
  (mapcar (rcurry #'close :abort abort) (tcp-listener-list-clients tcp-listener))
  (clrhash (tcp-listener-connections tcp-listener))
  (when (tcp-client-local-p (tcp-listener-tcp-client tcp-listener))
    (delete-file (pathname (tcp-client-local-name (tcp-listener-tcp-client tcp-listener)))))
  (close (tcp-listener-tcp-client tcp-listener) :abort abort)
  (unregister-socket tcp-listener)
  (let ((*tcp-listener* tcp-listener))
    (on-tcp-listener-close (tcp-listener-driver tcp-listener))))

(defun tcp-listener-paused-p (tcp-listener)
  "Returns true when `tcp-listener` is not currently accepting new connections."
  (tcp-client-paused-p (tcp-listener-tcp-client tcp-listener)))

(defun tcp-listener-pause (tcp-listener &key timeout)
  "Pauses read events for `tcp-listener`, preventing it from accepting new connections. While paused, all
   `tcp-client`s already connected to `tcp-listener` continue to work normally, but new connections will not
   be accepted. Incoming clients are not immediately turned away, although they may time out while
   connecting if the tcp-listener is paused for too long. If `timeout` is provided, it is interpreted as
   the number of seconds to wait before starting to accept clients again. This function has no
   effect if `tcp-listener` is already paused, although the resume timeout will still be activated."
  (tcp-client-pause (tcp-listener-tcp-client tcp-listener))
  (when timeout
    (add-timer (curry #'tcp-listener-resume tcp-listener)
               timeout :one-shot-p t)))

(defun tcp-listener-resume (tcp-listener)
  "Resumes read events for `tcp-listener`, enabling new client connections. If `tcp-listener` was already
   accepting clients, this function has no effect."
  (when (tcp-client-paused-p (tcp-listener-tcp-client tcp-listener))
    (iolib:set-io-handler
     (tcp-listener-event-base tcp-listener)
     (iolib:socket-os-fd (tcp-client-internal-socket (tcp-listener-tcp-client tcp-listener)))
     :read (lambda (&rest ig)
             (declare (ignore ig))
             (when-let (client-sock (iolib:accept-connection (tcp-client-internal-socket
                                                              (tcp-listener-tcp-client tcp-listener))))
               (let ((tcp-client (make-tcp-client (tcp-listener-client-driver tcp-listener)
                                          :external-format-in (tcp-listener-external-format-in tcp-listener)
                                          :external-format-out (tcp-listener-external-format-out tcp-listener))))
                 (setf (gethash tcp-client (tcp-listener-connections tcp-listener)) tcp-client
                       (tcp-client-internal-socket tcp-client) client-sock
                       (tcp-client-tcp-listener tcp-client) tcp-listener)
                 (let ((*tcp-listener* tcp-listener))
                   (on-tcp-listener-connection (tcp-listener-driver tcp-listener) tcp-client))
                 (tcp-client-resume tcp-client)
                 (start-writes tcp-client)))))))

(defun tcp-listen (driver &key
                   (host iolib:+ipv4-unspecified+)
                   (port (unless (pathnamep host) 1337))
                   (external-format-in *default-external-format*)
                   (external-format-out *default-external-format*)
                   (client-driver driver))
  "Starts a TCP listener. If successful, returns a `tcp-listener` object.

   * `driver` -- An instance of a driver class that will be used to dispatch `tcp-listener-event-driver` events. This driver will also be set as the `tcp-client-driver` for incoming connections.
   * `host` -- Either a string representing a local IP address to bind to, or a pathname to use as a unix socket.
   * `port` -- Port to listen on. Should not be provided if `host` is a unix socket.
   * `external-format-in` -- Default external-format-in for client connections.
   * `external-format-out` -- Default external-format-out for client connections."
  (let* ((tcp-listener (make-tcp-listener driver
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
         (tcp-client (make-tcp-client (tcp-listener-driver tcp-listener))))
    (setf (tcp-client-internal-socket tcp-client) internal-socket
          (tcp-listener-tcp-client tcp-listener) tcp-client)
    (register-socket tcp-client)
    (let ((*tcp-listener* tcp-listener))
      (on-tcp-listener-listen (tcp-listener-driver tcp-listener)))
    (tcp-listener-resume tcp-listener)
    tcp-listener))
