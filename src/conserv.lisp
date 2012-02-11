(in-package #:conserv)

;; Basic events
(defprotocol event-driver ()
 ((on-listen (driver)
   :default-form nil
   :documentation "Event called when the server has just started listening.")
  (on-error (driver error)
   :default-form nil
   :documentation "Event called when the server has experienced some error. ERROR is the actual
                   condition. This event is executed immediately before the server and all its
                   clients are shut down.")
  (on-connect (driver client)
   :default-form nil
   :documentation "Event called immediately after a new CLIENT has connected to the server.")
  (on-close (driver)
   :default-form nil
   :documentation "Event called immediately after the server has been shut down.")
  (on-data (driver client data)
   :default-form nil
   :documentation "Event called when CLIENT has received new data. If SERVER-BINARY-P is true, DATA
                   will be an array of (UNSIGNED-BYTE 8). Otherwise, DATA will be a string formatted
                   according to SERVER-EXTERNAL-FORMAT-IN.")
  #+nil(on-timeout (driver client) :default-form nil :documentation "TODO")
  (on-client-error (driver client error)
   :default-form nil
   :documentation "Event called when CLIENT has experienced some error. ERROR is the actual
                   condition. This event is executed immediately before the client is shut down.")
  (on-client-close (driver client)
   :default-form nil
   :documentation "Event called when CLIENT has been disconnected.")
  (on-drain (driver client)
   :default-form nil
   :documentation "Event called when CLIENT's output queue is empty."))
 (:documentation "Event drivers should define methods for the functions they're interested in
                  reacting to. All protocol methods are optional and do nothing by default.")
 (:prefix nil))

;; Client protocol
(defparameter *max-buffer-size* 16384)
(defprotocol client ()
  ((remote-name :reader)
   (remote-port :reader)
   (socket :reader)
   (server :reader)
   (read-buffer :reader)
   (write-queue :reader)
   (write-buffer :accessor)
   (write-buffer-offset :accessor)
   (bytes-read :accessor)
   (bytes-written :accessor)))

;; Server protocol
(defvar *default-external-format* :utf8)
(defprotocol server ()
  ((external-format-in :reader)
   (external-format-out :reader)
   (driver :reader)
   (binary-p :accessor)
   (event-base :accessor)
   (connections :accessor)
   (socket :accessor)))

(defun server-list-clients (server)
  (hash-table-values (server-connections server)))
(defun server-count-clients (server)
  (hash-table-count (server-connections server)))

(defun server-name (server)
  "Returns either the server's bound host, or the filename of the local socket."
  ;; TODO - local sockets
  (iolib:local-name (server-socket server)))

(defun server-port (server)
  "It's an error to call this on a local socket."
  ;; TODO - Return NIL instead of erroring?
  (iolib:local-port (server-socket server)))

;;;
;;; Base client
;;;
(defclass client (trivial-gray-stream-mixin
                  fundamental-binary-output-stream
                  fundamental-character-output-stream)
  ((socket :initarg :socket :reader client-socket)
   (server :initarg :server :reader client-server)
   (remote-name :initarg :rname :reader client-remote-name)
   (remote-port :initarg :rport :reader client-remote-port)
   (read-buffer :reader client-read-buffer)
   (write-queue :initform (make-queue) :reader client-write-queue)
   (write-buffer :initform nil :accessor client-write-buffer)
   (write-buffer-offset :initform 0 :accessor client-write-buffer-offset)
   (bytes-read :initform 0 :accessor client-bytes-read)
   (bytes-written :initform 0 :accessor client-bytes-written)))
(defun make-client (server socket remote-name remote-port &optional (buffer-size *max-buffer-size*))
  (let ((client (make-instance 'client :server server :socket socket :rname remote-name :rport remote-port)))
    (setf (slot-value client 'read-buffer) (make-array buffer-size :element-type 'flex:octet))
    client))

;;; Gray streams implementation
(defmethod stream-write-sequence ((client client) sequence start end &key)
  (enqueue (subseq sequence start end) (client-write-queue client)))
(defmethod stream-line-column ((client client))
  ;; TODO
  0)
(defmethod stream-write-char ((client client) character)
  ;; TODO - Meh. Maybe a buffer for very short messages or something?
  (enqueue (make-string 1 :initial-element character) (client-write-queue client)))
(defmethod stream-write-byte ((client client) byte)
  (enqueue (make-array 1 :element-type 'flex:octet :initial-element byte) (client-write-queue client)))
(defmethod stream-write-string ((client client) string &optional start end)
  (stream-write-sequence client string start end))
(defmethod close ((client client) &key abort)
  (iolib:remove-fd-handlers (server-event-base (client-server client))
                            (iolib:socket-os-fd (client-socket client)) :read t :write t :error t)
  (close (client-socket client) :abort abort)
  (remhash client (server-connections (client-server client)))
  (on-client-close (server-driver (client-server client)) client))

;;;
;;; Basic server
;;;
(defclass server ()
  ((driver :initarg :driver :reader server-driver)
   (external-format-in :initarg :external-format-in :reader server-external-format-in)
   (external-format-out :initarg :external-format-out :reader server-external-format-out)
   (binaryp :initarg :binaryp :reader server-binary-p)
   (event-base :accessor server-event-base)
   (connections :accessor server-connections)
   (socket :accessor server-socket)))

(defun make-server (driver
                    &key
                    binaryp
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*))
  (make-instance 'server
                 :driver driver
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :binaryp binaryp))

(defmethod close ((server server) &key abort)
  (maphash-values (rcurry #'close :abort abort) (server-connections server))
  (close (server-event-base server))
  (on-close (server-driver server)))


;;;
;;; Read
;;;
(defun client-pause (client &aux (server (client-server client)))
  (iolib:remove-fd-handlers (server-event-base server)
                            (iolib:socket-os-fd (client-socket client))
                            :read t))

(defun client-resume (client &aux (server (client-server client)))
  (iolib:set-io-handler (server-event-base server) (iolib:socket-os-fd (client-socket client))
                        :read (lambda (&rest ig)
                                (declare (ignore ig))
                                (handler-case
                                    (let* ((buffer (client-read-buffer client))
                                           (bytes-read
                                            (nth-value
                                             1 (iolib:receive-from (client-socket client) :buffer buffer))))
                                      (when (zerop bytes-read)
                                        (error 'end-of-file))
                                      (incf (client-bytes-read client) bytes-read)
                                      (on-data (server-driver server) client
                                               (if (server-binary-p server)
                                                   (subseq buffer 0 bytes-read)
                                                   (flex:octets-to-string buffer
                                                                          :start 0
                                                                          :end bytes-read
                                                                          :external-format (server-external-format-in server)))))
                                  ((or iolib:socket-connection-reset-error end-of-file) (e)
                                    (on-client-error (server-driver server) client e) (close client :abort t))))))

;;;
;;; Write
;;;
(defun content->buffer (server client content)
  "Given CONTENT, which can be any lisp data, converts that data to an array of '(unsigned-byte 8)"
  (declare (ignore client)) ; Eventually want to make this a genfun.
  (etypecase content
    ((simple-array flex:octet)
     content)
    (string
     (flex:string-to-octets content :external-format (server-external-format-out server)))
    ((or (array flex:octet) (cons flex:octet))
     (map-into (make-array (length content) :element-type 'flex:octet)
               content))))

(defun ensure-write-buffer (server client)
  (unless (client-write-buffer client)
    (setf (client-write-buffer client) (when-let (content (dequeue (client-write-queue client)))
                                         (content->buffer server client content))
          (client-write-buffer-offset client) 0)))

(defun start-writes (server client &aux (driver (server-driver server)))
  (iolib:set-io-handler
   (server-event-base server) (iolib:socket-os-fd (client-socket client))
   :write
   (lambda (&rest ig)
     (declare (ignore ig))
     (loop
        (ensure-write-buffer server client)
        (when (client-write-buffer client)
          (handler-case
              (let ((bytes-written (iolib:send-to (client-socket client)
                                                  (client-write-buffer client)
                                                  :start (client-write-buffer-offset client))))
                (incf (client-bytes-written client) bytes-written)
                (when (>= (incf (client-write-buffer-offset client) bytes-written)
                          (length (client-write-buffer client)))
                  (setf (client-write-buffer client) nil)
                  (when (queue-empty-p (client-write-queue client))
                    (on-drain driver client))))
            ((or iolib:socket-connection-reset-error isys:ewouldblock isys:epipe) (e)
              (on-client-error driver client e) (close client :abort t))))
        (when (queue-empty-p (client-write-queue client))
          (return))))))

;;;
;;; Listener
;;;
(defun server-listen (server &key (host iolib:+ipv4-loopback+) (port 1337))
  (unwind-protect
       (iolib:with-open-socket (server-sock :connect :passive
                                            :address-family :internet
                                            :type :stream
                                            :ipv6 nil
                                            :local-host host
                                            :local-port port
                                            :reuse-address t
                                            :backlog 5)
         (setf (server-event-base server) (make-instance 'iolib:event-base)
               (server-connections server) (make-hash-table)
               (server-socket server) server-sock)
         (on-listen (server-driver server))
         (iolib:set-io-handler (server-event-base server) (iolib:socket-os-fd server-sock)
                               :read (lambda (&rest ig)
                                       (declare (ignore ig))
                                       (when-let (client-sock (iolib:accept-connection server-sock))
                                         (multiple-value-bind (name port)
                                             (iolib:remote-name client-sock)
                                           (let ((client (make-client server client-sock name port)))
                                             (setf (gethash client (server-connections server)) client)
                                             (on-connect (server-driver server) client)
                                             (client-resume client)
                                             (start-writes server client))))))
         (handler-case
             (iolib:event-dispatch (server-event-base server))
           ((or iolib:socket-connection-reset-error iolib:hangup end-of-file) (e)
             (on-error (server-driver server) e))))
    (close server)))
