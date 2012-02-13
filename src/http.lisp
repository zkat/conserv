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
  ((data ((driver a) request data)
    :default-form nil)
   (close ((driver a) request)
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
  ((headers ((reply a)) :accessorp t)))

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
                                  :socket socket)))
      (format t "~&Request ready!~%")
      (describe request)
      (close socket :abort t)
      (when rest-of-data
        (on-request-data server request rest-of-data))
      (lambda (data)
        (on-request-data server request data)))))

(defun collect-message (server socket data &optional previous
                      &aux (concatenated (concatenate 'string previous data)))
  ;; TODO - Should try to accept bogus newlines eventually.
  (if-let ((match (search '(#\return #\linefeed #\return #\linefeed) concatenated)))
    (let ((message (subseq concatenated 0 (+ match 4)))
          (rest (subseq concatenated (+ match 4))))
      (handle-message server socket message (when (plusp (length rest)) rest)))
    (lambda (data)
      (collect-message server socket data concatenated))))

(defmethod on-tcp-server-connection ((driver http-server-driver) server socket)
  (format t "~&Got an http client connection.~%")
  (setf (socket-continuation driver socket) (curry 'collect-message server socket)))

(defmethod on-socket-data ((driver http-server-driver) socket data)
  (when-let (k (socket-continuation driver socket))
    (setf (socket-continuation driver socket) (funcall k data))))

(defun make-http-server (driver)
  ;; TODO - external formats, request- and reply-drivers.
  (make-server (make-instance 'http-server-driver :driver driver)))
