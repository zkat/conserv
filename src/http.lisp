(in-package #:conserv)

;;;
;;; Protocols
;;;
(defprotocol http-server-event-driver (a)
  ((listen ((driver a) server)
    :default-form nil)
   (request ((driver a) server request reply)
    :default-form nil)
   #+nil(connection ((driver a) server socket))
   (close ((driver a) server)
    :default-form nil)
   (continue ((driver a) server request reply)
    :default-form nil)
   (upgrade ((driver a) server request socket head)
    :default-form nil)
   (error ((driver a) server error)
    :default-form nil))
  (:prefix on-http-))

(defprotocol http-server (a)
  ((driver ((server a))
    :accessorp t))
  (:prefix http-server-))

(defprotocol request-event-driver (a)
  ((data ((driver a) request reply data)
    :default-form nil)
   (close ((driver a) request reply)
    :default-form nil))
  (:prefix on-request-))

(defprotocol request (a)
  ((method ((request a)))
   (url ((request a)))
   (headers ((request a)))
   #+nil(trailers ((request a)))
   (http-version ((request a)))
   (external-format ((request a)) :accessorp t)
   (socket ((request a))))
  (:prefix request-))

(defprotocol reply-event-driver (a)
  ((close ((driver a) reply)
    :default-form nil))
  (:prefix on-reply-))

(defprotocol reply (a)
  ((status ((reply a)) :accessorp t)
   (headers ((reply a)) :accessorp t)
   (write-headers ((reply a)))
   (headers-written-p ((reply a)) :accessorp t)
   (socket ((reply a))))
  (:prefix reply-))

(defclass request ()
  ((method :initarg :method :reader request-method)
   (url :initarg :url :reader request-url)
   (headers :initarg :headers :reader headers)
   (http-version :initarg :version :reader request-http-version)
   (external-format :initarg :external-format :accessor request-external-format)
   (socket :initarg :socket :reader request-socket)))

(defun parse-request-message (string)
  (destructuring-bind (request-line &rest header-lines)
      (cl-ppcre:split "[\\r\\n]+" string)
    (destructuring-bind (method url version)
        (cl-ppcre:split "\\s+" request-line :limit 3)
      (values method url version (loop for header-line in header-lines
                                    for header = (let ((line (string-trim " " header-line)))
                                                   (unless (zerop (length line))
                                                     (when-let (colon-pos (position #\: line :test #'char=))
                                                       (cons (subseq line 0 colon-pos)
                                                             (string-trim " " (subseq line (1+ colon-pos)))))))
                                    when header collect header)))))

(defclass reply (trivial-gray-stream-mixin
                 fundamental-binary-output-stream
                 fundamental-character-output-stream)
  ((headers :accessor reply-headers :initform nil)
   (status :accessor reply-status :initform 200)
   (socket :reader reply-socket :initarg :socket)
   (headers-written-p :accessor reply-headers-written-p :initform nil)))

(defmethod close ((reply reply) &key abort)
  (unless (or (reply-headers-written-p reply) abort)
    (write-headers reply))
  (close (reply-socket reply) :abort abort))

(defun ensure-headers-written (reply)
  (unless (reply-headers-written-p reply)
    (write-headers reply)))

(defmethod stream-write-sequence ((reply reply) sequence start end &key)
  (ensure-headers-written reply)
  (stream-write-sequence (reply-socket reply) sequence start end))
(defmethod stream-line-column ((reply reply))
  (ensure-headers-written reply)
  (stream-line-column (reply-socket reply)))
(defmethod stream-write-char ((reply reply) char)
  (ensure-headers-written reply)
  (stream-write-char (reply-socket reply) char))
(defmethod stream-write-byte ((reply reply) byte)
  (ensure-headers-written reply)
  (stream-write-byte (reply-socket reply) byte))
(defmethod stream-write-string ((reply reply) string &optional start end)
  (ensure-headers-written reply)
  (stream-write-string (reply-socket reply) string start end))

(defparameter +crlf+ #.(make-array 2 :element-type 'character :initial-contents '(#\return #\linefeed)))
(defmethod write-headers ((reply reply))
  (cond ((reply-headers-written-p reply)
         (warn "Headers already written."))
        (t
         (let ((socket (reply-socket reply)))
           (format socket "HTTP/1.1 ~A" (reply-status reply))
           (write-sequence +crlf+ socket)
           (loop for (name . value) in (reply-headers reply)
              do (format socket "~A: ~A" name value)
              (write-sequence +crlf+ socket))
           (write-sequence +crlf+ socket)
           (setf (reply-headers-written-p reply) t)))))

;;; Server
(defclass http-server-driver ()
  ((driver :initarg :driver :accessor http-server-driver)
   (continuations :initform (make-hash-table) :accessor client-continuations)))

(defun socket-continuation (driver socket)
  (gethash socket (client-continuations driver)))
(defun (setf socket-continuation) (k driver socket)
  (setf (gethash socket (client-continuations driver)) k))

(defun handle-message (server socket message-string rest-of-data)
  (multiple-value-bind (method url http-version headers)
      (parse-request-message message-string)
    (let ((request (make-instance 'request
                                  :method method
                                  :url url
                                  :version http-version
                                  :headers headers
                                  :socket socket
                                  :external-format (server-external-format-in server)))
          (reply (make-instance 'reply :socket socket))
          (driver (http-server-driver (server-driver server))))
      (on-http-request driver server request reply)
      (when rest-of-data
        (on-request-data driver request reply rest-of-data))
      (lambda (data)
        (on-request-data driver request reply data)))))

(defun collect-message (server socket data &optional previous
                      &aux (concatenated (concatenate 'string previous data)))
  ;; TODO - Should try to accept bogus newlines eventually.
  (if-let ((match (search '(#\return #\linefeed #\return #\linefeed) concatenated)))
    (let ((message (subseq concatenated 0 (+ match 4)))
          (rest (subseq concatenated (+ match 4))))
      (handle-message server socket message (when (plusp (length rest)) rest)))
    (lambda (data)
      (collect-message server socket data concatenated))))

(defmethod on-server-connection ((driver http-server-driver) server socket)
  (setf (socket-continuation driver socket) (curry 'collect-message server socket)))

(defmethod on-socket-data ((driver http-server-driver) socket data)
  (when-let (k (socket-continuation driver socket))
    (setf (socket-continuation driver socket) (funcall k data))))

(defun http-listen (driver &key
                    (host iolib:+ipv4-unspecified+)
                    (port 8080)
                    (external-format-in *default-external-format*)
                    (external-format-out *default-external-format*)
                    binaryp)
  (server-listen (make-instance 'http-server-driver :driver driver)
                 :host host
                 :port port
                 :external-format-in external-format-in
                 :external-format-out external-format-out
                 :binaryp binaryp))
