(in-package #:conserv)

;; Basic events
(defprotocol event-driver ()
 ((server-listen (driver)
   :default-form nil
   :documentation "Event called when the server has just started listening.")
  (server-error (driver error)
   :default-form (invoke-debugger error)
   :documentation "Event called when the server has experienced some error. ERROR is the actual
                   condition. This event is executed immediately before the server and all its
                   clients are shut down.")
  (server-close (driver)
   :default-form nil
   :documentation "Event called immediately after the server has been shut down.")
  (client-connect (driver client)
   :default-form nil
   :documentation "Event called immediately after a new CLIENT has connected to the server.")
  (client-data (driver client data)
   :default-form nil
   :documentation "Event called when CLIENT has received new data. If SERVER-BINARY-P is true, DATA
                   will be an array of (UNSIGNED-BYTE 8). Otherwise, DATA will be a string formatted
                   according to SERVER-EXTERNAL-FORMAT-IN.")
  #+nil(timeout (driver client) :default-form nil :documentation "TODO")
  (client-error (driver error client)
   :default-form (progn (warn "Dropping client ~S after error condition ~S. ~
                               (Do you want to redefine ON-CLIENT-ERROR?)"
                              client error)
                        (drop-connection error))
   :documentation "Event called when CLIENT has experienced some error. ERROR is the actual
                   condition. This event is executed immediately before the client is shut down.
                   By default, this event simply drops the client connection.

                   The fact that ON-CLIENT-ERROR receives the actual condition allows a sort of
                   condition handling by specializing both the driver and the condition. For
                   example:
                   ;; The default DROP-CONNECTION
                   (defmethod on-client-error ((driver my-driver) (error end-of-file) client)
                     (format t \"~&Got an end of file.~%\")
                     (drop-connection error))
                   (defmethod on-client-error ((driver my-driver) (error blood-and-guts) client)
                     (format t \"~&Oh, the humanity! Let the error kill the whole server :(~%\"))")
  (client-close (driver client)
   :default-form nil
   :documentation "Event called when CLIENT has been disconnected.")
  (client-output-empty (driver client)
   :default-form nil
   :documentation "Event called when CLIENT's output queue is empty."))
 (:documentation "Event drivers should define methods for the functions they're interested in
                  reacting to. All protocol methods are optional and do nothing by default.")
 (:prefix on-))

(defun drop-connection (&optional condition)
  "Can only be called within the scope of ON-CLIENT-ERROR."
  (let ((r (find-restart 'drop-connection condition)))
    (when r (invoke-restart r))))

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
   (socket :accessor)
   (datagram-p :reader)))

(defun server-list-clients (server)
  (hash-table-values (server-connections server)))
(defun server-count-clients (server)
  (hash-table-count (server-connections server)))

(defun server-socket-local-p (server)
  "Returns TRUE if the server's socket is a local socket (aka a unix/IPC socket)."
  (ecase (iolib:socket-address-family (server-socket server))
    ((:local :file)
     t)
    ((:internet :ipv6 :ipv4)
     nil)))

(defun server-name (server)
  "Returns either the server's bound host, or the filename of the local socket."
  (iolib:local-name (server-socket server)))

(defun server-port (server)
  "Local sockets have no port, so this returns NIL."
  (unless (server-socket-local-p server)
    (iolib:local-port (server-socket server))))

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
   (socket :accessor server-socket)
   (datagramp :initarg :datagramp :reader server-socket-datagram-p)))

(defun make-server (driver
                    &key
                    binaryp
                    datagramp
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*))
  (make-instance 'server
                 :driver driver
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :binaryp binaryp
                 :datagramp datagramp))

(defmethod close ((server server) &key abort)
  (when (slot-boundp server 'connections)
    (maphash-values (rcurry #'close :abort abort) (server-connections server)))
  (when (slot-boundp server 'event-base)
    (close (server-event-base server)))
  (when (and (slot-boundp server 'socket)
             (server-socket server))
    (when (server-socket-local-p server)
      (delete-file (server-name server))))
  (on-server-close (server-driver server)))

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
                                ;; NOTE - The redundant errors are there for reference.
                                (handler-bind (((or iolib:socket-connection-reset-error
                                                    end-of-file
                                                    error)
                                                (lambda (e)
                                                  (on-client-error (server-driver server) e client))))
                                  (restart-case
                                      (let* ((buffer (client-read-buffer client))
                                             (bytes-read
                                              (nth-value
                                               1 (iolib:receive-from (client-socket client) :buffer buffer))))
                                        (when (zerop bytes-read)
                                          (error 'end-of-file))
                                        (incf (client-bytes-read client) bytes-read)
                                        (let ((data (if (server-binary-p server)
                                                        (subseq buffer 0 bytes-read)
                                                        (flex:octets-to-string buffer
                                                                               :start 0
                                                                               :end bytes-read
                                                                               :external-format (server-external-format-in server)))))
                                          (on-client-data (server-driver server) client data)))
                                    (continue () nil)
                                    (drop-connection () (close client :abort t)))))))

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
          ;; TODO - the errors are mostly there for my own reference.
          (handler-bind (((or iolib:socket-connection-reset-error
                              isys:ewouldblock
                              isys:epipe
                              error)
                          (lambda (e)
                            (on-client-error driver e client))))
            (restart-case
                (let ((bytes-written (iolib:send-to (client-socket client)
                                                    (client-write-buffer client)
                                                    :start (client-write-buffer-offset client))))
                  (incf (client-bytes-written client) bytes-written)
                  (when (>= (incf (client-write-buffer-offset client) bytes-written)
                            (length (client-write-buffer client)))
                    (setf (client-write-buffer client) nil)
                    (when (queue-empty-p (client-write-queue client))
                      (on-client-output-empty driver client))))
              (continue () nil)
              (drop-connection () (close client :abort t)))))
        (when (queue-empty-p (client-write-queue client))
          (return))))))

;;;
;;; Listener
;;;
(defun server-listen (server &key (host iolib:+ipv4-loopback+) (port 1337))
  (unwind-protect
       (let ((args
              `(:connect :passive
                :address-family ,(if (pathnamep host)
                                     :local
                                     :internet)
                :type ,(if (server-socket-datagram-p server)
                           :datagram
                           :stream)
                ,@(unless (server-socket-datagram-p server)`(:backlog 5))
                :ipv6 nil
                ,(if (pathnamep host)
                     :local-filename
                     :local-host)
                ,(if (pathnamep host)
                     (namestring host)
                     host)
                ,@(unless (pathnamep host)
                    `(:local-port ,port))
                :reuse-address t)))
         (with-open-stream (server-sock (apply #'iolib:make-socket args))
           (setf (server-event-base server) (make-instance 'iolib:event-base)
                 (server-connections server) (make-hash-table)
                 (server-socket server) server-sock)
           (on-server-listen (server-driver server))
           (iolib:set-io-handler (server-event-base server) (iolib:socket-os-fd server-sock)
                                 :read (lambda (&rest ig)
                                         (declare (ignore ig))
                                         (when-let (client-sock (iolib:accept-connection server-sock))
                                           (multiple-value-bind (name port)
                                               (iolib:remote-name client-sock)
                                             (let ((client (make-client server client-sock name port)))
                                               (setf (gethash client (server-connections server)) client)
                                               (on-client-connect (server-driver server) client)
                                               (client-resume client)
                                               (start-writes server client))))))
           (handler-bind (((or iolib:socket-connection-reset-error
                               iolib:hangup
                               end-of-file
                               error)
                           (lambda (e)
                             (on-server-error (server-driver server) e))))
             ;; TODO - in order to have a non-blocking thing, server-listen will need to be
             ;; restructured and this will need to be extracted out into a separate function.
             ;; TODO - Add a restart that will simply continue to the next iteration.
             (iolib:event-dispatch (server-event-base server)))))
    (close server)))
