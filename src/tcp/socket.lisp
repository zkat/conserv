(in-package #:conserv.tcp)

(deftype octet ()
  '(unsigned-byte 8))

;; Events
(defvar *socket*)
(setf (documentation '*socket* 'variable)
      "During execution of socket events, this is bound to the `socket` object associated with the
       current event. This variable is unbound outside the scope of `socket-event-driver` events.")

(defprotocol socket-event-driver (a)
  ((error (driver error)
    :default-form nil
    :documentation "Event called when `*socket*` has experienced some error. `error` is the actual
                    condition. This event is executed immediately before the client is shut down.
                    By default, this event simply drops the client connection.

                    The fact that `on-socket-error` receives the actual condition allows a sort of
                    condition handling by specializing both the driver and the condition. For
                    example:
                    ```lisp
                    (defmethod on-socket-error ((driver my-driver) (error something-harmless))
                      (format t \"~&Nothing to see here.~%\"))

                    (defmethod on-socket-error ((driver my-driver) (error blood-and-guts))
                      (format t \"~&Oh, the humanity!~%\")
                      (drop-connection error))
                    ```")
   (end-of-file (driver)
    :default-form nil
    :documentation "Event called when an EOF has been received while reading from `*socket*`")
   (connect ((driver a))
    :default-form nil
    :documentation "Event called immediately after a successful `socket-connect`.")
   (data ((driver a) data)
    :default-form nil
    :documentation "Event called when `*socket*` has received some new
                    `DATA`. If `(socket-external-format-in *socket*)` is `nil`, `data` will by an
                    array of `(unsigned-byte 8)`. Otherwise, it will be used to encode the incoming
                    data before passing it to `on-socket-data`")
   (close ((driver a))
    :default-form nil
    :documentation "Event called when `*socket*` has been disconnected.")
   (output-empty ((driver a))
    :default-form nil
    :documentation "Event called when `*socket*`'s output queue is empty."))
  (:prefix on-socket-)
  (:documentation "Defines the base API for standard sockets."))

(defun drop-connection (&optional condition)
  "Drops the current connection and allows execution to continue. Can only be called within the
   scope of ON-SOCKET-ERROR."
  (let ((r (find-restart 'drop-connection condition)))
    (when r (invoke-restart r))))

;; Base socket protocol
(defprotocol socket (a)
  ((driver ((socket a))
    :accessorp t
    :documentation "Driver object used to dispatch `socket`'s events.")
   (server ((socket a))
    :accessorp t
    :documentation "Holds the associated server object if this socket was accepted by a server.")
   (internal-socket ((socket a))
    :accessorp t
    :documentation "Internal IOLib socket for this conserv socket.")
   (read-buffer ((socket a)))
   (write-queue ((socket a)))
   (write-buffer ((socket a))
    :accessorp t)
   (write-buffer-offset ((socket a))
    :accessorp t)
   ;; TODO - Make it an accessor so buffer sizes can be dynamically changed by users.
   #+nil(buffer-size :accessor)
   (bytes-read ((socket a))
    :accessorp t
    :documentation "Total bytes successfully read by `socket` so far.")
   (bytes-written ((socket a))
    :accessorp t
    :documentation "Total bytes successfully written to `socket`. Note that this is incremented only
                    after the data has been written to the file descriptor, not after each call to a
                    stream writing function.")
   (external-format-in ((socket a))
    :accessorp t
    :documentation "External format to use when converting incoming octets into characters. If `nil`,
                    no encoding will be done on incoming data, and `on-socket-data` will receive the
                    raw `(unsigned-byte 8)` data.")
   (external-format-out ((socket a))
    :accessorp t
    :documentation "External format to use for outgoing octets and strings. If `nil`, an error is
                    signaled if an attempt is made to write a string to `socket`.")
   (close-after-drain-p ((socket a))
    :accessorp t
    :documentation "When true, the internal socket will be closed once the socket's output buffer is
                    drained.")
   (reading-p ((socket a))
    :accessorp t
    :documentation "When true, read events are being monitored.")
   (writing-p ((socket a))
    :accessorp t
    :documentation "When true, write events are being monitored."))
  (:prefix socket-))

;;; Implementation
(defvar *max-buffer-size* 16384)
(defclass socket (trivial-gray-stream-mixin
                  fundamental-binary-output-stream
                  fundamental-character-output-stream)
  ((driver :initarg :driver :accessor socket-driver)
   (server :initform nil :accessor socket-server)
   (internal-socket :accessor socket-internal-socket)
   (read-buffer :reader socket-read-buffer)
   (write-queue :initform (make-queue) :reader socket-write-queue)
   (write-buffer :initform nil :accessor socket-write-buffer)
   (write-buffer-offset :initform 0 :accessor socket-write-buffer-offset)
   (bytes-read :initform 0 :accessor socket-bytes-read)
   (bytes-written :initform 0 :accessor socket-bytes-written)
   (external-format-in :initarg :external-format-in :accessor socket-external-format-in)
   (external-format-out :initarg :external-format-out :accessor socket-external-format-out)
   (close-after-drain-p :initform nil :accessor socket-close-after-drain-p)
   (readingp :initform nil :accessor socket-reading-p)
   (writingp :initform nil :accessor socket-writing-p)))
(defun make-socket (driver &key
                    (buffer-size *max-buffer-size*)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*))
  (let ((socket (make-instance 'socket
                               :driver driver
                               :external-format-in external-format-in
                               :external-format-out external-format-out)))
    (setf (slot-value socket 'read-buffer) (make-array buffer-size :element-type 'octet))
    socket))

(defun socket-event-base (socket)
  (declare (ignore socket))
  (or *event-base*
      (error "Operation not supported outside of the scope of WITH-EVENT-LOOP.")))

(defun socket-enqueue (sequence socket)
  (prog1 (enqueue sequence (socket-write-queue socket))
    (start-writes socket)))

;;; Gray streams implementation
(defmethod stream-write-sequence ((socket socket) sequence start end &key
                                  &aux (length (length sequence)))
  (socket-enqueue (if (and (or (null start)
                               (eq start 0))
                           (or (null end)
                               (eq end length)))
                      sequence
                      (subseq sequence (or start 0) (or end length)))
                  socket))
(defmethod stream-line-column ((socket socket))
  ;; TODO
  0)
(defmethod stream-write-char ((socket socket) character)
  ;; TODO - Meh. Maybe a buffer for very short messages or something?
  (socket-enqueue (make-string 1 :initial-element character) socket))
(defmethod stream-write-byte ((socket socket) byte)
  (socket-enqueue (make-array 1 :element-type 'octet :initial-element byte) socket))
(defmethod stream-write-string ((socket socket) string &optional start end)
  (stream-write-sequence socket string start end))
(defmethod close ((socket socket) &key abort)
  (when-let (evbase (and (slot-boundp socket 'internal-socket)
                         (socket-event-base socket)))
    (socket-pause socket)
    (cond (abort
           (finish-close socket))
          (t
           (setf (socket-close-after-drain-p socket) t)
           nil))))

(defun finish-close (socket)
  (when-let (evbase (and (slot-boundp socket 'internal-socket)
                         (socket-event-base socket)))
    (pause-writes socket)
    (when-let (server (socket-server socket))
      (remhash socket (server-connections server)))
    (close (socket-internal-socket socket) :abort t)
    (unregister-socket socket))
  (let ((*socket* socket))
    (on-socket-close (socket-driver socket)))
  t)

(defun socket-connect (driver host &key
                       (port 0)
                       (wait t)
                       (buffer-size *max-buffer-size*)
                       (external-format-in *default-external-format*)
                       (external-format-out *default-external-format*))
  "Establishes a TCP connection with `host`. If successful, returns a `socket` object.

   * `driver` -- an instance of a driver class that will be used to dispatch `socket-event-driver` events.
   * `host` -- Either a string representing a remote IP address or hostname, or a pathname to a local IPC socket.
   * `port` -- Port number to connect to. Should not be provided if `host` is an IPC socket.
   * `external-format-in` -- Either an external format or `nil`. Used for determining the encoding of data passed to `on-socket-data`
   * `external-format-out` -- Either an external format or `nil`. Used for converting strings written to the socket. If `nil`, an error will be signaled if an attempt is made to write a string to the socket."
  (let ((socket (make-socket driver
                             :buffer-size buffer-size
                             :external-format-in external-format-in
                             :external-format-out external-format-out))
        (internal-socket (iolib:make-socket :connect :active
                                            :address-family (if (pathnamep host)
                                                                :local
                                                                :internet)
                                            :ipv6 nil)))
    (handler-bind ((error (lambda (e &aux (*socket* socket))
                            (on-socket-error (socket-driver socket) e))))
      (restart-case
          (iolib:connect internal-socket (if (pathnamep  host)
                                             (iolib:make-address (namestring host))
                                             (iolib:lookup-hostname host))
                         :port port :wait wait)
        (drop-connection () (close socket :abort t))))
    (register-socket socket)
    (setf (socket-internal-socket socket) internal-socket)
    (socket-resume socket)
    (start-writes socket)
    socket))

(defun socket-local-p (socket)
  "Returns true if `socket` is connected to a local IPC socket, `nil` otherwise."
  (ecase (iolib:socket-address-family (socket-internal-socket socket))
    ((:local :file)
     t)
    ((:internet :ipv4 :ipv6)
     nil)))

;; TODO - document how these behave when `socket` is local.
(defun socket-remote-name (socket)
  "Name of the remote host `socket` is connected to."
  (iolib:remote-name (socket-internal-socket socket)))
(defun socket-remote-port (socket)
  "Remote port that `socket` is connected to."
  (iolib:remote-port (socket-internal-socket socket)))
(defun socket-local-name (socket)
  "Local host name for `socket`'s connection."
  (if (socket-local-p socket)
      (iolib:address-name (iolib:local-name (socket-internal-socket socket)))
      (iolib:local-host (socket-internal-socket socket))))
(defun socket-local-port (socket)
  "Local port for `socket`'s connection."
  (unless (socket-local-p socket)
    (iolib:local-port (socket-internal-socket socket))))

;;; Reading
(defun socket-paused-p (socket)
  "True when `socket` is paused, i.e. it is not currently waiting for read events."
  (not (socket-reading-p socket)))

(defun socket-pause (socket &key timeout)
  "Pauses read events for `socket`. While a socket is paused, it will continue to send data as it is
   queued, but it will not listen for and handle read events (meaning, on-socket-data will not be
   called). This can be useful for throttling clients. If `timeout` is provided, it is interpreted
   as the number of seconds to wait before resuming reads on the socket. Has no effect if `socket`
   is already paused, although the resume timeout will still be activated."
  (unless (socket-paused-p socket)
    (iolib:remove-fd-handlers (socket-event-base socket)
                              (iolib:socket-os-fd (socket-internal-socket socket))
                              :read t)
    (setf (socket-reading-p socket) nil)
    (when timeout
      (add-timer (curry #'socket-resume socket) timeout :one-shot t))))

(defun socket-resume (socket)
  "Resumes reads on a paused `socket`. If `socket` was not already paused, this function has no
   effect."
  (when (socket-paused-p socket)
    (iolib:set-io-handler (socket-event-base socket)
                          (iolib:socket-os-fd (socket-internal-socket socket))
                          :read (lambda (&rest ig)
                                  (declare (ignore ig))
                                  (handler-bind ((end-of-file (lambda (e &aux (*socket* socket))
                                                                (on-socket-end-of-file
                                                                 (socket-driver socket))
                                                                (drop-connection e)))
                                                 (error
                                                  (lambda (e &aux (*socket* socket))
                                                    (on-socket-error (socket-driver socket) e))))
                                    (restart-case
                                        (let* ((buffer (socket-read-buffer socket))
                                               (bytes-read
                                                (nth-value
                                                 1 (iolib:receive-from (socket-internal-socket socket) :buffer buffer))))
                                          (when (zerop bytes-read)
                                            (error 'end-of-file :stream socket))
                                          (incf (socket-bytes-read socket) bytes-read)
                                          (let ((data (if-let (format (socket-external-format-in socket))
                                                        (babel:octets-to-string buffer
                                                                                :start 0
                                                                                :end bytes-read
                                                                                :encoding format)
                                                        (subseq buffer 0 bytes-read))))
                                            (let ((*socket* socket))
                                              (on-socket-data (socket-driver socket) data))))
                                      (continue () nil)
                                      (drop-connection () (close socket :abort t))))))
    (setf (socket-reading-p socket) t)))

;;; Writing
(defun content->buffer (socket content)
  "Given CONTENT, which can be any lisp data, converts that data to an array of '(unsigned-byte 8)"
  (etypecase content
    ((simple-array octet)
     content)
    (string
     (if-let (format (socket-external-format-out socket))
       (babel:string-to-octets content :encoding format)
       (error "Cannot write strings to socket ~A. SOCKET-EXTERNAL-FORMAT-OUT is NIL." socket)))
    ((or (array octet) (cons octet))
     (map-into (make-array (length content) :element-type 'octet)
               content))))

(defun coalesce-outputs (socket)
  (let* ((outputs (dequeue-all (socket-write-queue socket)))
         (total (reduce #'+ outputs :key #'length)))
    (loop with buffer = (make-array total :element-type 'octet)
       with index = 0
       for output in outputs
       do (loop for byte across (content->buffer socket output)
             do (setf (aref buffer index) byte)
               (incf index))
       finally (return buffer))))

(defun ensure-write-buffer (socket)
  (or (socket-write-buffer socket)
      (setf (socket-write-buffer-offset socket) 0
            (socket-write-buffer socket) (coalesce-outputs socket))))

(defun pause-writes (socket)
  (when (socket-writing-p socket)
    (iolib:remove-fd-handlers (socket-event-base socket)
                              (iolib:socket-os-fd (socket-internal-socket socket))
                              :write t)
    (setf (socket-writing-p socket) nil)))

(defun start-writes (socket &aux (driver (socket-driver socket)))
  (unless (socket-writing-p socket)
    (iolib:set-io-handler
     (socket-event-base socket) (iolib:socket-os-fd (socket-internal-socket socket))
     :write
     (lambda (&rest ig)
       (declare (ignore ig))
       (loop
          (when-let (buffer (ensure-write-buffer socket))
            ;; TODO - the errors are mostly there for my own reference.
            (handler-bind (((or iolib:socket-connection-reset-error
                                isys:ewouldblock
                                isys:epipe
                                error)
                            (lambda (e &aux (*socket* socket))
                              (on-socket-error driver e))))
              (restart-case
                  (let ((bytes-written (iolib:send-to (socket-internal-socket socket)
                                                      buffer
                                                      :start (socket-write-buffer-offset socket))))
                    (incf (socket-bytes-written socket) bytes-written)
                    (when (>= (incf (socket-write-buffer-offset socket) bytes-written)
                              (length (socket-write-buffer socket)))
                      (setf (socket-write-buffer socket) nil)
                      (when (queue-empty-p (socket-write-queue socket))
                        (cond ((socket-close-after-drain-p socket)
                               (finish-close socket))
                              (t
                               (pause-writes socket)
                               (let ((*socket* socket))
                                 (on-socket-output-empty driver)))))))
                (continue () nil)
                (drop-connection () (close socket :abort t)))))
          (when (queue-empty-p (socket-write-queue socket))
            (return)))))
    (setf (socket-writing-p socket) t)))
