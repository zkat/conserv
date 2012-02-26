(in-package #:conserv.http)

(defvar *reply*)

(defprotocol reply-event-driver (a)
  ((close ((driver a))
    :default-form nil))
  (:prefix on-reply-))

(defprotocol reply (a)
  ((status ((reply a)) :accessorp t)
   (headers ((reply a)) :accessorp t)
   (header-bytes ((reply a)) :accessorp t)
   (write-headers ((reply a)))
   (headers-written-p ((reply a)) :accessorp t)
   (chunked-p ((reply a)) :accessorp t)
   (http-server ((reply a)))
   (external-format ((reply a)) :accessorp t)
   (request ((reply a)))
   (socket ((reply a)))
   #+nil(trailers ((reply a)) :accessorp t))
  (:prefix reply-))

;;;
;;; Headers
;;;
(defparameter +http-1.1+ (babel:string-to-octets "HTTP/1.1 " :encoding :ascii))
(defparameter +crlf-octets+ #.(make-array 2
                                          :element-type '(unsigned-byte 8)
                                          :initial-contents #(13 10)))

(defun format-header (stream name value)
  (format stream "~:(~A~): ~A~A" name value +crlf-ascii+))

(defun calculate-header-string (header-alist)
  (with-output-to-string (s)
    (loop for (name . value) in header-alist
       with transfer-encoding-p
       with content-length-p
       do (format-header s name value)
         (when (eq :content-length name)
           (setf content-length-p t))
         (when (eq :transfer-encoding name)
           (setf transfer-encoding-p t))
       finally (progn
                 (unless (or transfer-encoding-p
                             content-length-p)
                   (format-header s :transfer-encoding "chunked"))
                 (write-sequence +crlf-ascii+ s)))))

;; NOTE - the reason this isn't just (setf reply-headers) is because some implementations expand
;;        setf functions into something like (let ((val ,value)) (funcall #'(setf foo) val)), which
;;        will always pass 'val as the literal argument to the compiler macro. :(
(defun set-headers (reply &rest headers &aux (alist (plist-alist headers)))
  (setf (reply-header-bytes reply) (babel:string-to-octets
                                      (calculate-header-string alist)
                                      :encoding :ascii)
        (reply-headers reply) alist))

#+nil
(defun add-headers (reply &rest headers &aux (alist (plist-alist headers)))
  ;; TODO - in order for this to work, we can't append the final crlf in
  ;;        calculate-header-string. The logic behind automatically inserting :transfer-encoding
  ;;        needs to be changed, too.
  (setf (reply-header-bytes reply) (concatenate '(vector (unsigned-byte 8))
                                                (reply-header-bytes reply)
                                                (babel:string-to-octets
                                                 (calculate-header-string alist)
                                                 :encoding :ascii)))
  (appendf (reply-headers reply) alist))

(defun write-headers (reply)
  (cond ((reply-headers-written-p reply)
         (warn "Headers already written."))
        (t
         (let ((socket (reply-socket reply)))
           (write-sequence +http-1.1+ socket)
           (write-sequence (babel:string-to-octets (princ-to-string (reply-status reply))
                                                   :encoding :ascii)
                           socket)
           (write-sequence +crlf-octets+ socket)
           (write-sequence (reply-header-bytes reply)
                           (reply-socket reply))
           (setf (reply-headers-written-p reply) t)))))

(defun ensure-headers-written (reply)
  (unless (reply-headers-written-p reply)
    (write-headers reply)))

;;; Optimization
(defun process-literal-headers (headers env)
  (loop
     for (name value) on headers by #'cddr
     if (and (or (symbolp name) (stringp  name))
             (constantp value env))
     collect (format-header nil name value) into compiled
     and collect name into literal and collect value into literal
     else
     collect name into dynamic and collect value into dynamic
     finally (return (values (when compiled
                               (apply #'babel:concatenate-strings-to-octets :ascii compiled))
                             literal
                             dynamic))))

(define-compiler-macro set-headers (reply &rest headers &environment env)
  (multiple-value-bind (compiled literal dynamic)
      (process-literal-headers headers env)
    (let ((dynamics-var (gensym "DYNAMIC-HEADERS"))
          (literals-var (gensym "LITERAL-HEADERS"))
          (reply-var (gensym "REPLY")))
      `(let ((,dynamics-var (plist-alist (list ,@dynamic)))
             (,literals-var (plist-alist (list ,@literal)))
             (,reply-var ,reply))
         (setf (reply-header-bytes ,reply-var)
               ,(cond ((and literal dynamic)
                       `(concatenate '(vector (unsigned-byte 8))
                                     ,compiled
                                     (babel:string-to-octets
                                      (calculate-header-string ,dynamics-var)
                                      :encoding :ascii)))
                      (compiled
                       (concatenate '(vector (unsigned-byte 8))
                                    compiled
                                    (babel:string-to-octets
                                     (calculate-header-string nil)
                                     :encoding :ascii)))
                      (dynamic `(babel:string-to-octets
                                 (calculate-header-string ,dynamics-var)
                                 :encoding :ascii)))
               ;; NOTE - nconc is *probably* okay here. literals and dynamics have already been
               ;;        used above...
               (reply-headers ,reply-var)
               (nconc ,literals-var ,dynamics-var))))))

(defclass reply (trivial-gray-stream-mixin
                 fundamental-binary-output-stream
                 fundamental-character-output-stream)
  ((headers :accessor reply-headers :initform '((:transfer-encoding . "chunked")))
   (header-bytes :accessor reply-header-bytes
                 :initform #.(babel:concatenate-strings-to-octets
                              :ascii "Transfer-Encoding: chunked"
                              +crlf-ascii+))
   (status :accessor reply-status :initform 200)
   (socket :reader reply-socket :initarg :socket)
   (request :reader reply-request :initarg :request)
   (headers-written-p :accessor reply-headers-written-p :initform nil)
   (http-server :reader reply-http-server :initarg :http-server)
   (chunkedp :accessor reply-chunked-p :initform t)
   (external-format :accessor reply-external-format :initarg :external-format)))

(defmethod (setf reply-headers) :after (new-headers (reply reply))
  (when (member :content-length new-headers :key #'car :test #'string-equal)
    (setf (reply-chunked-p reply) nil)))

;;; Reply Gray Streams
(defmethod stream-write-sequence ((reply reply) sequence start end &key)
  (ensure-headers-written reply)
  (let ((socket (reply-socket reply))
        (chunkedp (reply-chunked-p reply))
        (output (etypecase sequence
                  (string
                   (babel:string-to-octets sequence :encoding (reply-external-format reply)))
                  (sequence sequence))))
    (when chunkedp
      (write-sequence (babel:string-to-octets
                       (format nil "~x~a" (- (or end (length output)) (or start 0)) +crlf-ascii+)
                       :encoding :ascii)
                      socket))
    (write-sequence output socket :start start :end end)
    (when chunkedp
      (write-sequence +crlf-octets+ socket))))
(defmethod stream-line-column ((reply reply))
  (stream-line-column (reply-socket reply)))
(defmethod stream-write-char ((reply reply) char)
  (write-sequence (string char) reply))
(defmethod stream-write-byte ((reply reply) byte)
  (write-sequence (make-array 1 :element-type '(unsigned-byte 8) :initial-element byte) reply))
(defmethod stream-write-string ((reply reply) string &optional start end)
  (stream-write-sequence reply string start end))

(defmethod close ((reply reply) &key abort)
  (unless (or (reply-headers-written-p reply) abort)
    (write-headers reply))
  (when (reply-chunked-p reply)
    (write-sequence #.(make-array 5 :element-type '(unsigned-byte 8)
                                  :initial-contents #(48 13 10 13 10))
                    (reply-socket reply)))
  (close (reply-request reply) :abort abort))
