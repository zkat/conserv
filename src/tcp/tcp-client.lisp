(in-package #:conserv.tcp)

(deftype octet ()
  '(unsigned-byte 8))

;; Events
(defvar *tcp-client*)
(setf (documentation '*tcp-client* 'variable)
      "During execution of tcp-client events, this is bound to the `tcp-client` object associated with the
       current event. This variable is unbound outside the scope of `tcp-client-event-driver` events.")

(defprotocol tcp-client-event-driver (a)
  ((error (driver error)
    :default-form nil
    :documentation "Event called when `*tcp-client*` has experienced some error. `error` is the actual
                    condition. This event is executed immediately before the client is shut down.
                    By default, this event simply drops the client connection.

                    The fact that `on-tcp-client-error` receives the actual condition allows a sort of
                    condition handling by specializing both the driver and the condition. For
                    example:
                    ```lisp
                    (defmethod on-tcp-client-error ((driver my-driver) (error something-harmless))
                      (format t \"~&Nothing to see here.~%\"))

                    (defmethod on-tcp-client-error ((driver my-driver) (error blood-and-guts))
                      (format t \"~&Oh, the humanity!~%\")
                      (drop-connection error))
                    ```")
   (end-of-file (driver)
    :default-form nil
    :documentation "Event called when an EOF has been received while reading from `*tcp-client*`")
   (connect ((driver a))
    :default-form nil
    :documentation "Event called immediately after a successful `tcp-client-connect`.")
   (data ((driver a) data)
    :default-form nil
    :documentation "Event called when `*tcp-client*` has received some new
                    `DATA`. If `(tcp-client-external-format-in *tcp-client*)` is `nil`, `data` will by an
                    array of `(unsigned-byte 8)`. Otherwise, it will be used to encode the incoming
                    data before passing it to `on-tcp-client-data`")
   (close ((driver a))
    :default-form nil
    :documentation "Event called when `*tcp-client*` has been disconnected.")
   (output-empty ((driver a))
    :default-form nil
    :documentation "Event called when `*tcp-client*`'s output queue is empty."))
  (:prefix on-tcp-client-)
  (:documentation "Defines the base API for standard tcp-clients."))

(defun drop-connection (&optional condition)
  "Drops the current connection and allows execution to continue. Can only be called within the
   scope of ON-TCP-CLIENT-ERROR."
  (let ((r (find-restart 'drop-connection condition)))
    (when r (invoke-restart r))))

;; Base tcp-client protocol
(defprotocol tcp-client (a)
  ((driver ((tcp-client a))
    :accessorp t
    :documentation "Driver object used to dispatch `tcp-client`'s events.")
   (tcp-listener ((tcp-client a))
    :accessorp t
    :documentation "Holds the associated tcp-listener object if this tcp-client was accepted by a tcp-listener.")
   (internal-socket ((tcp-client a))
    :accessorp t
    :documentation "Internal IOLib socket for this conserv tcp-client.")
   (read-buffer ((tcp-client a)))
   (write-queue ((tcp-client a)))
   (write-buffer ((tcp-client a))
    :accessorp t)
   (write-buffer-offset ((tcp-client a))
    :accessorp t)
   ;; TODO - Make it an accessor so buffer sizes can be dynamically changed by users.
   #+nil(buffer-size :accessor)
   (bytes-read ((tcp-client a))
    :accessorp t
    :documentation "Total bytes successfully read by `tcp-client` so far.")
   (bytes-written ((tcp-client a))
    :accessorp t
    :documentation "Total bytes successfully written to `tcp-client`. Note that this is incremented only
                    after the data has been written to the file descriptor, not after each call to a
                    stream writing function.")
   (external-format-in ((tcp-client a))
    :accessorp t
    :documentation "External format to use when converting incoming octets into characters. If `nil`,
                    no encoding will be done on incoming data, and `on-tcp-client-data` will receive the
                    raw `(unsigned-byte 8)` data.")
   (external-format-out ((tcp-client a))
    :accessorp t
    :documentation "External format to use for outgoing octets and strings. If `nil`, an error is
                    signaled if an attempt is made to write a string to `tcp-client`.")
   (close-after-drain-p ((tcp-client a))
    :accessorp t
    :documentation "When true, the internal tcp-client will be closed once the tcp-client's output buffer is
                    drained.")
   (reading-p ((tcp-client a))
    :accessorp t
    :documentation "When true, read events are being monitored.")
   (writing-p ((tcp-client a))
    :accessorp t
    :documentation "When true, write events are being monitored."))
  (:prefix tcp-client-))

;;; Implementation
(defvar *max-buffer-size* 16384)
(defclass tcp-client (trivial-gray-stream-mixin
                  fundamental-binary-output-stream
                  fundamental-character-output-stream)
  ((driver :initarg :driver :accessor tcp-client-driver)
   (tcp-listener :initform nil :accessor tcp-client-tcp-listener)
   (internal-socket :accessor tcp-client-internal-socket)
   (read-buffer :reader tcp-client-read-buffer)
   (write-queue :initform (make-queue) :reader tcp-client-write-queue)
   (write-buffer :initform nil :accessor tcp-client-write-buffer)
   (write-buffer-offset :initform 0 :accessor tcp-client-write-buffer-offset)
   (bytes-read :initform 0 :accessor tcp-client-bytes-read)
   (bytes-written :initform 0 :accessor tcp-client-bytes-written)
   (external-format-in :initarg :external-format-in :accessor tcp-client-external-format-in)
   (external-format-out :initarg :external-format-out :accessor tcp-client-external-format-out)
   (close-after-drain-p :initform nil :accessor tcp-client-close-after-drain-p)
   (readingp :initform nil :accessor tcp-client-reading-p)
   (writingp :initform nil :accessor tcp-client-writing-p)))
(defun make-tcp-client (driver &key
                    (buffer-size *max-buffer-size*)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*))
  (let ((tcp-client (make-instance 'tcp-client
                               :driver driver
                               :external-format-in external-format-in
                               :external-format-out external-format-out)))
    (setf (slot-value tcp-client 'read-buffer) (make-array buffer-size :element-type 'octet))
    tcp-client))

(defun tcp-client-event-base (tcp-client)
  (declare (ignore tcp-client))
  (or *event-base*
      (error "Operation not supported outside of the scope of `with-event-loop`")))

(defun tcp-client-enqueue (sequence tcp-client)
  (prog1 (enqueue sequence (tcp-client-write-queue tcp-client))
    (start-writes tcp-client)))

;;; Gray streams implementation
(defmethod stream-write-sequence ((tcp-client tcp-client) sequence start end &key
                                  &aux (length (length sequence)))
  (tcp-client-enqueue (if (and (or (null start)
                               (eq start 0))
                           (or (null end)
                               (eq end length)))
                      sequence
                      (subseq sequence (or start 0) (or end length)))
                  tcp-client))
(defmethod stream-line-column ((tcp-client tcp-client))
  ;; TODO
  0)
(defmethod stream-write-char ((tcp-client tcp-client) character)
  ;; TODO - Meh. Maybe a buffer for very short messages or something?
  (tcp-client-enqueue (make-string 1 :initial-element character) tcp-client))
(defmethod stream-write-byte ((tcp-client tcp-client) byte)
  (tcp-client-enqueue (make-array 1 :element-type 'octet :initial-element byte) tcp-client))
(defmethod stream-write-string ((tcp-client tcp-client) string &optional start end)
  (stream-write-sequence tcp-client string start end))
(defmethod close ((tcp-client tcp-client) &key abort)
  (when-let (evbase (and (slot-boundp tcp-client 'internal-socket)
                         (tcp-client-event-base tcp-client)))
    (tcp-client-pause tcp-client)
    (cond (abort
           (finish-close tcp-client))
          (t
           (setf (tcp-client-close-after-drain-p tcp-client) t)
           nil))))

(defun finish-close (tcp-client)
  (when-let (evbase (and (slot-boundp tcp-client 'internal-socket)
                         (tcp-client-event-base tcp-client)))
    (pause-writes tcp-client)
    (when-let (tcp-listener (tcp-client-tcp-listener tcp-client))
      (remhash tcp-client (tcp-listener-connections tcp-listener)))
    (close (tcp-client-internal-socket tcp-client) :abort t)
    (unregister-socket tcp-client))
  (let ((*tcp-client* tcp-client))
    (on-tcp-client-close (tcp-client-driver tcp-client)))
  t)

(defun tcp-connect
    (driver host &key
     (port 0)
     (wait t)
     (buffer-size *max-buffer-size*)
     (external-format-in *default-external-format*)
     (external-format-out *default-external-format*))
  "Establishes a TCP connection with `host`. If successful, returns a `tcp-client` object.

   * `driver` -- an instance of a driver class that will be used to dispatch `tcp-client-event-driver`
     events.
   * `host` -- Either a string representing a remote IP address or hostname, or a pathname to a
     local IPC tcp-client.
   * `port` -- Port number to connect to. Should not be provided if `host` is an IPC tcp-client.
   * `external-format-in` -- Either an external format or `nil`. Used for determining the encoding
     of data passed to `on-tcp-client-data`
   * `external-format-out` -- Either an external format or `nil`. Used for converting strings
     written to the tcp-client. If `nil`, an error will be signaled if an attempt is made to write a
     string to the tcp-client."
  (let ((tcp-client (make-tcp-client driver
                             :buffer-size buffer-size
                             :external-format-in external-format-in
                             :external-format-out external-format-out))
        (internal-socket (iolib:make-socket :connect :active
                                            :address-family (if (pathnamep host)
                                                                :local
                                                                :internet)
                                            :ipv6 nil)))
    (handler-bind ((error (lambda (e &aux (*tcp-client* tcp-client))
                            (on-tcp-client-error (tcp-client-driver tcp-client) e))))
      (restart-case
          (iolib:connect internal-socket (if (pathnamep  host)
                                             (iolib:make-address (namestring host))
                                             (iolib:lookup-hostname host))
                         :port port :wait wait)
        (drop-connection () (close tcp-client :abort t))))
    (register-socket tcp-client)
    (setf (tcp-client-internal-socket tcp-client) internal-socket)
    (tcp-client-resume tcp-client)
    (start-writes tcp-client)
    tcp-client))

(defun tcp-client-local-p (tcp-client)
  "Returns true if `tcp-client` is connected to a local IPC socket, `nil` otherwise."
  (ecase (iolib:socket-address-family (tcp-client-internal-socket tcp-client))
    ((:local :file)
     t)
    ((:internet :ipv4 :ipv6)
     nil)))

;; TODO - document how these behave when `tcp-client` is local.
(defun tcp-client-remote-name (tcp-client)
  "Name of the remote host `tcp-client` is connected to."
  (iolib:remote-name (tcp-client-internal-socket tcp-client)))
(defun tcp-client-remote-port (tcp-client)
  "Remote port that `tcp-client` is connected to."
  (iolib:remote-port (tcp-client-internal-socket tcp-client)))
(defun tcp-client-local-name (tcp-client)
  "Local host name for `tcp-client`'s connection."
  (if (tcp-client-local-p tcp-client)
      (iolib:address-name (iolib:local-name (tcp-client-internal-socket tcp-client)))
      (iolib:local-host (tcp-client-internal-socket tcp-client))))
(defun tcp-client-local-port (tcp-client)
  "Local port for `tcp-client`'s connection."
  (unless (tcp-client-local-p tcp-client)
    (iolib:local-port (tcp-client-internal-socket tcp-client))))

;;; Reading
(defun tcp-client-paused-p (tcp-client)
  "True when `tcp-client` is paused, i.e. it is not currently waiting for read events."
  (not (tcp-client-reading-p tcp-client)))

(defun tcp-client-pause (tcp-client &key timeout)
  "Pauses read events for `tcp-client`. While a tcp-client is paused, it will continue to send data as it is
   queued, but it will not listen for and handle read events (meaning, on-tcp-client-data will not be
   called). This can be useful for throttling clients. If `timeout` is provided, it is interpreted
   as the number of seconds to wait before resuming reads on the tcp-client. Has no effect if `tcp-client`
   is already paused, although the resume timeout will still be activated."
  (unless (tcp-client-paused-p tcp-client)
    (iolib:remove-fd-handlers (tcp-client-event-base tcp-client)
                              (iolib:socket-os-fd (tcp-client-internal-socket tcp-client))
                              :read t)
    (setf (tcp-client-reading-p tcp-client) nil)
    (when timeout
      (add-timer (curry #'tcp-client-resume tcp-client)
                 timeout :one-shot-p t))))

(defun tcp-client-resume (tcp-client)
  "Resumes reads on a paused `tcp-client`. If `tcp-client` was not already paused, this function has no
   effect."
  (when (tcp-client-paused-p tcp-client)
    (iolib:set-io-handler
     (tcp-client-event-base tcp-client)
     (iolib:socket-os-fd (tcp-client-internal-socket tcp-client))
     :read (lambda (&rest ig)
             (declare (ignore ig))
             (handler-bind ((end-of-file (lambda (e &aux (*tcp-client* tcp-client))
                                           (on-tcp-client-end-of-file
                                            (tcp-client-driver tcp-client))
                                           (drop-connection e)))
                            (error
                             (lambda (e &aux (*tcp-client* tcp-client))
                               (on-tcp-client-error (tcp-client-driver tcp-client) e))))
               (restart-case
                   (let* ((buffer (tcp-client-read-buffer tcp-client))
                          (bytes-read
                           (nth-value
                            1 (iolib:receive-from (tcp-client-internal-socket tcp-client) :buffer buffer))))
                     (when (zerop bytes-read)
                       (error 'end-of-file :stream tcp-client))
                     (incf (tcp-client-bytes-read tcp-client) bytes-read)
                     (let ((data (if-let (format (tcp-client-external-format-in tcp-client))
                                   (babel:octets-to-string buffer
                                                           :start 0
                                                           :end bytes-read
                                                           :encoding format)
                                   (subseq buffer 0 bytes-read))))
                       (let ((*tcp-client* tcp-client))
                         (on-tcp-client-data (tcp-client-driver tcp-client) data))))
                 (continue () nil)
                 (drop-connection () (close tcp-client :abort t))))))
    (setf (tcp-client-reading-p tcp-client) t)))

;;; Writing

(defun content->buffer (tcp-client content)
  "Given CONTENT, which can be any lisp data, converts that data to an array of '(unsigned-byte 8)"
  (etypecase content
    ((simple-array octet)
     content)
    (string
     (if-let (format (tcp-client-external-format-out tcp-client))
       (babel:string-to-octets content :encoding format)
       (error "Cannot write strings to tcp-client ~A. TCP-CLIENT-EXTERNAL-FORMAT-OUT is NIL." tcp-client)))
    ((or (array octet) (cons octet))
     (map-into (make-array (length content) :element-type 'octet)
               content))))

(defun coalesce-outputs (tcp-client)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let* ((outputs (mapcar (curry #'content->buffer tcp-client)
                          (dequeue-all (tcp-client-write-queue tcp-client))))
         (total (reduce #'+ outputs :key #'length)))
    (declare (fixnum total))
    (let ((buffer (make-array total :element-type 'octet))
          (next-start 0))
      (declare (type fixnum next-start))
      (loop for output of-type (simple-array octet) in outputs
         do (replace buffer output :start1 next-start)
           (incf next-start (length output)))
      buffer)))

;; NOTE - Since we delay conversion of tcp-client output, we have to make sure that if the external
;; format is changed, anything that was written to the tcp-client is encoded using the format that was
;; present when the write happened.
(defmethod (setf tcp-client-external-format-out) :before (new-value (tcp-client tcp-client))
  (declare (ignore new-value))
  (tcp-client-enqueue (coalesce-outputs tcp-client) tcp-client))

(defun ensure-write-buffer (tcp-client)
  (or (tcp-client-write-buffer tcp-client)
      (setf (tcp-client-write-buffer-offset tcp-client) 0
            (tcp-client-write-buffer tcp-client) (coalesce-outputs tcp-client))))

(defun pause-writes (tcp-client)
  (when (tcp-client-writing-p tcp-client)
    (iolib:remove-fd-handlers (tcp-client-event-base tcp-client)
                              (iolib:socket-os-fd (tcp-client-internal-socket tcp-client))
                              :write t)
    (setf (tcp-client-writing-p tcp-client) nil)))

(defun start-writes (tcp-client &aux (driver (tcp-client-driver tcp-client)))
  (unless (tcp-client-writing-p tcp-client)
    (iolib:set-io-handler
     (tcp-client-event-base tcp-client)
     (iolib:socket-os-fd (tcp-client-internal-socket tcp-client))
     :write
     (lambda (&rest ig)
       (declare (ignore ig))
       (loop
          (when-let (buffer (ensure-write-buffer tcp-client))
            ;; TODO - the errors are mostly there for my own reference.
            (handler-bind (((or iolib:socket-connection-reset-error
                                isys:ewouldblock
                                isys:epipe
                                error)
                            (lambda (e &aux (*tcp-client* tcp-client))
                              (on-tcp-client-error driver e))))
              (restart-case
                  (let ((bytes-written (iolib:send-to (tcp-client-internal-socket tcp-client)
                                                      buffer
                                                      :start (tcp-client-write-buffer-offset tcp-client))))
                    (incf (tcp-client-bytes-written tcp-client) bytes-written)
                    (when (>= (incf (tcp-client-write-buffer-offset tcp-client) bytes-written)
                              (length (tcp-client-write-buffer tcp-client)))
                      (setf (tcp-client-write-buffer tcp-client) nil)
                      (when (queue-empty-p (tcp-client-write-queue tcp-client))
                        (cond ((tcp-client-close-after-drain-p tcp-client)
                               (finish-close tcp-client))
                              (t
                               (pause-writes tcp-client)
                               (let ((*tcp-client* tcp-client))
                                 (on-tcp-client-output-empty driver)))))))
                (continue () nil)
                (drop-connection () (close tcp-client :abort t)))))
          (when (queue-empty-p (tcp-client-write-queue tcp-client))
            (return)))))
    (setf (tcp-client-writing-p tcp-client) t)))
