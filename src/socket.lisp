(in-package #:conserv)

;; Events
(defprotocol socket-event-driver ()
  ((error (driver socket error)
    :default-form (drop-connection error)
    :documentation "Event called when SOCKET has experienced some error. ERROR is the actual
                    condition. This event is executed immediately before the client is shut down.
                    By default, this event simply drops the client connection.

                    The fact that ON-SOCKET-ERROR receives the actual condition allows a sort of
                    condition handling by specializing both the driver and the condition. For
                    example:
                    ;; The default DROP-CONNECTION
                    (defmethod on-socket-error ((driver my-driver) socket (error end-of-file))
                      (format t \"~&Got an end of file.~%\")
                      (drop-connection error))
                    (defmethod on-socket-error ((driver my-driver) socket (error blood-and-guts))
                      (format t \"~&Oh, the humanity! Let the error kill the whole server :(~%\"))")
   (connect (driver socket)
    :default-form nil
    :documentation "Event called immediately after a successful SOCKET-CONNECT.")
   (data (driver socket data)
    :default-form nil
    :documentation "Event called when SOCKET has received some new DATA.")
   (close (driver socket)
    :default-form nil
    :documentation "Event called when SOCKET has been disconnected.")
   (output-empty (driver socket)
    :default-form nil
    :documentation "Event called when SOCKET's output queue is empty."))
  (:prefix on-socket-)
  (:documentation "Defines the base API for standard sockets."))

(defun drop-connection (&optional condition)
  "Can only be called within the scope of ON-SOCKET-ERROR."
  (let ((r (find-restart 'drop-connection condition)))
    (when r (invoke-restart r))))

;; Base socket protocol
(defprotocol socket (binary-output-stream character-output-stream)
  ((driver :reader
    :documentation "Driver object used to dispatch SOCKET's events.")
   (server :accessor
    :documentation "Holds the associated server object if this socket was accepted by a server.")
   (internal-socket :accessor
    :documentation "Internal IOLib socket for this conserv socket.")
   (read-buffer :reader)
   (write-queue :reader)
   (write-buffer :accessor)
   (write-buffer-offset :accessor)
   ;; TODO - Make it an accessor so buffer sizes can be dynamically changed by users.
   #+nil(buffer-size :accessor)
   (bytes-read :accessor)
   (bytes-written :accessor)
   (external-format-in :reader
    :documentation "External format to use when converting incoming octets into characters.")
   (external-format-out :reader
    :documentation "External format to use for outgoing octets and strings.")
   (binary-p :accessor
    :documentation "If true, incoming data will not be converted to strings. ON-SOCKET-DATA will
                    instead return the raw (UNSIGNED-BYTE 8) arrays. In this mode,
                    SOCKET-EXTERNAL-FORMAT-OUT will only be used when converting input strings for
                    output -- binary input data (as through WRITE-SEQUENCE) will not be
                    converted. SOCKET-EXTERNAL-FORMAT-IN is not used at all in binary mode.
                    This value can be changed after a socket has already been created.")))

;;; Implementation
(defvar *default-external-format* :utf8)
(defvar *max-buffer-size* 16384)
(defclass socket (trivial-gray-stream-mixin
                  fundamental-binary-output-stream
                  fundamental-character-output-stream)
  ((driver :initarg :driver :reader socket-driver)
   (server :initform nil :accessor socket-server)
   (internal-socket :accessor socket-internal-socket)
   (read-buffer :reader socket-read-buffer)
   (write-queue :initform (make-queue) :reader socket-write-queue)
   (write-buffer :initform nil :accessor socket-write-buffer)
   (write-buffer-offset :initform 0 :accessor socket-write-buffer-offset)
   (bytes-read :initform 0 :accessor socket-bytes-read)
   (bytes-written :initform 0 :accessor socket-bytes-written)
   (external-format-in :initarg :external-format-in :reader socket-external-format-in)
   (external-format-out :initarg :external-format-out :reader socket-external-format-out)
   (binary-p :initarg :binaryp :accessor socket-binary-p)))
(defun make-socket (driver &key
                    (buffer-size *max-buffer-size*)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*)
                    binaryp)
  (let ((socket (make-instance 'socket
                               :driver driver
                               :external-format-in external-format-in
                               :external-format-out external-format-out
                               :binaryp binaryp)))
    (setf (slot-value socket 'read-buffer) (make-array buffer-size :element-type 'flex:octet))
    socket))

(defun socket-event-base (socket)
  (declare (ignore socket))
  (or *event-base*
      (error "Operation not supported outside of the scope of WITH-EVENT-LOOP.")))

;;; Gray streams implementation
(defmethod stream-write-sequence ((socket socket) sequence start end &key)
  (enqueue (subseq sequence (or start 0) (or end (length sequence))) (socket-write-queue socket)))
(defmethod stream-line-column ((socket socket))
  ;; TODO
  0)
(defmethod stream-write-char ((socket socket) character)
  ;; TODO - Meh. Maybe a buffer for very short messages or something?
  (enqueue (make-string 1 :initial-element character) (socket-write-queue socket)))
(defmethod stream-write-byte ((socket socket) byte)
  (enqueue (make-array 1 :element-type 'flex:octet :initial-element byte) (socket-write-queue socket)))
(defmethod stream-write-string ((socket socket) string &optional start end)
  (stream-write-sequence socket string start end))
(defmethod close ((socket socket) &key abort)
  (when-let (evbase (and (slot-boundp socket 'internal-socket)
                         (socket-event-base socket)))
    (iolib:remove-fd-handlers evbase
                              (iolib:socket-os-fd (socket-internal-socket socket))
                              :read t :write t :error t)
    (close (socket-internal-socket socket) :abort abort)
    (when-let (server (socket-server socket))
      (remhash socket (server-connections server)))
    (unregister-socket socket)
    (on-socket-close (socket-driver socket) socket)))

(defun socket-connect (socket host &key (port 0) (wait t))
  (let ((internal-socket (iolib:make-socket :connect :active
                                            :address-family (if (pathnamep host)
                                                                :local
                                                                :internet)
                                            :ipv6 nil)))
    (handler-bind ((error (lambda (e)
                            (on-socket-error (socket-driver socket) socket e))))
      (restart-case
          (iolib:connect internal-socket (if (pathnamep  host)
                                             (iolib:make-address (namestring host))
                                             (iolib:lookup-hostname host)) :port port :wait wait)
        (drop-connection () (close socket :abort t))))
    (register-socket socket)
    (setf (socket-internal-socket socket) internal-socket)
    (socket-resume socket)
    (start-writes socket)
    socket))

(defun socket-local-p (socket)
  (ecase (iolib:socket-address-family (socket-internal-socket socket))
    ((:local :file)
     t)
    ((:internet :ipv4 :ipv6)
     nil)))

(defun socket-remote-name (socket)
  (iolib:remote-name (socket-internal-socket socket)))
(defun socket-remote-port (socket)
  (iolib:remote-port (socket-internal-socket socket)))
(defun socket-local-name (socket)
  (if (socket-local-p socket)
      (iolib:address-name (iolib:local-name (socket-internal-socket socket)))
      (iolib:local-host (socket-internal-socket socket))))
(defun socket-local-port (socket)
  (unless (socket-local-p socket)
    (iolib:local-port (socket-internal-socket socket))))

;;; Reading
(defun socket-paused-p (socket)
  (iolib.multiplex::fd-monitored-p (socket-event-base socket)
                                   (iolib:socket-os-fd (socket-internal-socket socket))
                                   :read))

(defun socket-pause (socket &key timeout)
  (iolib:remove-fd-handlers (socket-event-base socket)
                            (iolib:socket-os-fd (socket-internal-socket socket))
                            :read t)
  (when timeout
    (add-timer (curry #'socket-resume socket) timeout :one-shot t)))

(defun socket-resume (socket)
  (iolib:set-io-handler (socket-event-base socket)
                        (iolib:socket-os-fd (socket-internal-socket socket))
                        :read (lambda (&rest ig)
                                (declare (ignore ig))
                                ;; NOTE - The redundant errors are there for reference.
                                (handler-bind (((or iolib:socket-connection-reset-error
                                                    end-of-file
                                                    error)
                                                (lambda (e)
                                                  (on-socket-error (socket-driver socket) e socket))))
                                  (restart-case
                                      (let* ((buffer (socket-read-buffer socket))
                                             (bytes-read
                                              (nth-value
                                               1 (iolib:receive-from (socket-internal-socket socket) :buffer buffer))))
                                        (when (zerop bytes-read)
                                          (error 'end-of-file))
                                        (incf (socket-bytes-read socket) bytes-read)
                                        (let ((data (if (socket-binary-p socket)
                                                        (subseq buffer 0 bytes-read)
                                                        (flex:octets-to-string buffer
                                                                               :start 0
                                                                               :end bytes-read
                                                                               :external-format (socket-external-format-in socket)))))
                                          (on-socket-data (socket-driver socket) socket data)))
                                    (continue () nil)
                                    (drop-connection () (close socket :abort t)))))))

;;; Writing
(defun content->buffer (socket content)
  "Given CONTENT, which can be any lisp data, converts that data to an array of '(unsigned-byte 8)"
  (etypecase content
    ((simple-array flex:octet)
     content)
    (string
     (flex:string-to-octets content :external-format (socket-external-format-out socket)))
    ((or (array flex:octet) (cons flex:octet))
     (map-into (make-array (length content) :element-type 'flex:octet)
               content))))

(defun ensure-write-buffer (socket)
  (unless (socket-write-buffer socket)
    (setf (socket-write-buffer socket) (when-let (content (dequeue (socket-write-queue socket)))
                                         (content->buffer socket content))
          (socket-write-buffer-offset socket) 0)))

(defun start-writes (socket &aux (driver (socket-driver socket)))
  (iolib:set-io-handler
   (socket-event-base socket) (iolib:socket-os-fd (socket-internal-socket socket))
   :write
   (lambda (&rest ig)
     (declare (ignore ig))
     (loop
        (ensure-write-buffer socket)
        (when (socket-write-buffer socket)
          ;; TODO - the errors are mostly there for my own reference.
          (handler-bind (((or iolib:socket-connection-reset-error
                              isys:ewouldblock
                              isys:epipe
                              error)
                          (lambda (e)
                            (on-socket-error driver e socket))))
            (restart-case
                (let ((bytes-written (iolib:send-to (socket-internal-socket socket)
                                                    (socket-write-buffer socket)
                                                    :start (socket-write-buffer-offset socket))))
                  (incf (socket-bytes-written socket) bytes-written)
                  (when (>= (incf (socket-write-buffer-offset socket) bytes-written)
                            (length (socket-write-buffer socket)))
                    (setf (socket-write-buffer socket) nil)
                    (when (queue-empty-p (socket-write-queue socket))
                      (on-socket-output-empty driver socket))))
              (continue () nil)
              (drop-connection () (close socket :abort t)))))
        (when (queue-empty-p (socket-write-queue socket))
          (return))))))
