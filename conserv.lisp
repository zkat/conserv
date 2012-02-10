;;;; conserv.lisp

(in-package #:conserv)

;;; "conserv" goes here. Hacks and glory await!

;; ;; TCP server events
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

(defclass tcp-client (fundamental-character-output-stream)
  ((socket :initarg :socket :reader client-socket)
   (server :initarg :server :reader client-server)
   (remote-name :initarg :rname :reader client-remote-name)
   (remote-port :initarg :rport :reader client-remote-port)
   (read-buffer :reader client-read-buffer)
   (write-queue :initform (make-queue) :reader client-write-queue)
   (write-buffer :initform nil :accessor client-write-buffer)
   (write-buffer-offset :initform 0 :accessor client-write-buffer-offset)))
(defun make-tcp-client (server socket remote-name remote-port &optional (buffer-size *max-buffer-size*))
  (let ((client (make-instance 'tcp-client :server server :socket socket :rname remote-name :rport remote-port)))
    (setf (slot-value client 'read-buffer) (make-array buffer-size :element-type 'flex:octet))
    client))

(defvar *default-external-format* :utf8)
(defgeneric server-external-format-in (server))
(defgeneric server-driver (server))
(defgeneric server-binary-p (server))
(defgeneric server-event-base (server))
(defgeneric server-connections (server))
(defgeneric server-socket (server))
(defclass tcp-server ()
  ((driver :initarg :driver :reader server-driver)
   (external-format-in :initarg :external-format-in :reader server-external-format-in)
   (external-format-out :initarg :external-format-out :reader server-external-format-out)
   (binaryp :initarg :binaryp :reader server-binary-p)
   (event-base :reader server-event-base)
   (connections :reader server-connections)
   (socket :reader server-socket)))

(defun make-tcp-server (driver
                        &key
                        binaryp
                        (external-format-in *default-external-format*)
                        (external-format-out *default-external-format*))
  (make-instance 'tcp-server
                 :driver driver
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :binaryp binaryp))

(defun start-listener (server &key (host iolib:+ipv4-loopback+) (port 1337))
  (unwind-protect
       (iolib:with-open-socket (server-sock :connect :passive
                                            :address-family :internet
                                            :type :stream
                                            :ipv6 nil
                                            :local-host host
                                            :local-port port
                                            :reuse-address t
                                            :backlog 5)
         (with-slots (event-base connections socket) server
           (setf event-base (make-instance 'iolib:event-base)
                 connections (make-hash-table)
                 socket server-sock))
         (on-listen (server-driver server))
         (iolib:set-io-handler (server-event-base server) (iolib:socket-os-fd server-sock)
                               :read (lambda (&rest ig)
                                       (declare (ignore ig))
                                       (when-let (client-sock (iolib:accept-connection server-sock))
                                         (multiple-value-bind (name port)
                                             (iolib:remote-name client-sock)
                                           (let ((client (make-tcp-client server client-sock name port)))
                                             (setf (gethash client (server-connections server)) client)
                                             (on-connect (server-driver server) client)
                                             (start-reads server client)
                                             (start-writes server client))))))
         (handler-case
             (iolib:event-dispatch (server-event-base server))
           ((or iolib:socket-connection-reset-error iolib:hangup end-of-file) (e)
             (on-error (server-driver server) e))))
    (close server)))

(defmethod close ((server tcp-server) &key abort)
  (maphash-values (rcurry #'close :abort abort) (server-connections server))
  (close (server-event-base server))
  (on-close (server-driver server)))

(defun start-reads (server client &aux (driver (server-driver server)))
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
                                      (on-data driver client
                                               (if (server-binary-p server)
                                                   (subseq buffer 0 bytes-read)
                                                   (flex:octets-to-string buffer
                                                                          :start 0
                                                                          :end bytes-read
                                                                          :external-format (server-external-format-in server)))))
                                  ((or iolib:socket-connection-reset-error end-of-file) (e)
                                    (on-client-error driver client e) (close client :abort t))))))

(defun stop-reads (driver event-base client)
  (declare (ignore driver))
  (iolib:remove-fd-handlers event-base (iolib:socket-os-fd (client-socket client)) :read t))

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

(defmethod stream-write-sequence ((client tcp-client) sequence start end &key)
  (enqueue (subseq sequence start end) (client-write-queue client)))
(defmethod stream-line-column ((client tcp-client))
  ;; TODO
  0)
(defmethod stream-write-char ((client tcp-client) character)
  ;; TODO - Meh. Maybe a buffer for very short messages or something?
  (enqueue (string character) (client-write-queue client)))
(defmethod stream-write-string ((client tcp-client) string &optional start end)
  (stream-write-sequence client string start end))
(defmethod close ((client tcp-client) &key abort)
  (iolib:remove-fd-handlers (server-event-base (client-server client))
                            (iolib:socket-os-fd (client-socket client)) :read t :write t :error t)
  (close (client-socket client) :abort abort)
  (remhash client (server-connections (client-server client)))
  (on-client-close (server-driver (client-server client)) client))

;; TCP server test
(defclass echo () ())
(defmethod on-connect ((driver echo) client)
  (format t "~&Client connected. Host: ~A, Port: ~A~%" (client-remote-name client) (client-remote-port client))
  (format client "~&Welcome. Everything you say will be echoed back to you now.~%"))
(defmethod on-data ((driver echo) client data)
  (format t "~&Data length: ~S~%" (length data))
  (write-sequence data client))
(defmethod on-client-close ((driver echo) client)
  (format t "~&Client disconnected: ~S~%" client))
#+nil(start-listener (make-tcp-server (make-instance 'echo)) :host "127.0.0.1" :port 1337)
