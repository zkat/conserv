(defpackage #:conserv
  (:use #:cl
        #:alexandria
        #:trivial-gray-streams)
  (:export
   ;; Events
   #:on-listen
   #:on-error
   #:on-connect
   #:on-close
   #:on-data
   #:on-client-error
   #:on-drain
   ;; #:on-timeout ; Unimplemented

   ;; Client functions
   #:*max-buffer-size*
   #:client-remote-name
   #:client-remote-port
   #:client-server
   #:client-pause
   #:client-resume

   ;; Server functions
   #:make-server
   #:server-external-format-in
   #:server-external-format-out
   #:server-binary-p
   #:server-list-clients
   #:server-count-clients
   #:server-listen))

(in-package #:conserv)

;; Utils
(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun queue-empty-p (q)
  (null (car q)))

;; Basic events
(defmacro defevent (name lambda-list &rest options)
  `(defgeneric ,name ,lambda-list ,@options
               (:method ,lambda-list (declare (ignore ,@lambda-list)) nil)))
(defevent on-listen (driver))
(defevent on-error (driver error))
(defevent on-connect (driver client))
(defevent on-close (driver))
(defevent on-data (driver client data))
(defevent on-timeout (driver client))
(defevent on-client-error (driver client error))
(defevent on-client-close (driver client))
(defevent on-drain (driver client))

;; Client protocol
(defparameter *max-buffer-size* 16384)
(defgeneric client-remote-name (client))
(defgeneric client-remote-port (client))
(defgeneric client-socket (client))
(defgeneric client-server (client))
(defgeneric client-read-buffer (client))
(defgeneric client-write-queue (client))
(defgeneric client-write-buffer (client))
(defgeneric (setf client-write-buffer) (new-value client))
(defgeneric client-write-buffer-offset (client))
(defgeneric (setf client-write-buffer-offset) (new-value client))

;; Server protocol
(defvar *default-external-format* :utf8)
(defgeneric server-external-format-in (server))
(defgeneric server-external-format-out (server))
(defgeneric server-driver (server))
(defgeneric server-binary-p (server))
(defgeneric (setf server-binary-p) (new-value server))
(defgeneric server-event-base (server))
(defgeneric (setf server-event-base) (new-value server))
(defgeneric server-connections (server))
(defgeneric (setf server-connections) (new-value server))
(defgeneric server-socket (server))
(defgeneric (setf server-socket) (new-value server))

(defun server-list-clients (server)
  (hash-table-values (server-connections server)))
(defun server-count-clients (server)
  (hash-table-count (server-connections server)))

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
   (write-buffer-offset :initform 0 :accessor client-write-buffer-offset)))
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
