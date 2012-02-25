(in-package #:conserv.http)

(defstruct request-parser
  buffer
  (state :request-line)
  method
  url
  http-version
  headers)

(define-condition request-parser-error (error) ())

(defun feed-parser (parser data)
  (case (request-parser-state parser)
    (:request-line
     (handle-request-line parser data))
    (:header
     (handle-header parser data))
    (:done
     (values t parser data))))

(defun fill-buffer (parser data)
  (setf (request-parser-buffer parser)
        (if-let (buffer (request-parser-buffer parser))
          (concatenate 'string buffer data)
          data)))

(defparameter *request-line-regex*
  (cl-ppcre:create-scanner "(\\w+)\\s+([^\\s]+)\\s+(?:HTTP/(\\d\\.\\d))?"
                           :case-insensitive-mode t))

(defun handle-request-line (parser data)
  (let ((buffer (fill-buffer parser data)))
    (if-let (match (search '(#\return #\linefeed) buffer))
      (let ((request-line (subseq buffer 0 match))
            (rest (subseq buffer (+ match 2))))
        (multiple-value-bind (method url version)
            (parse-request-line request-line)
          (setf (request-parser-method parser) method
                (request-parser-url parser) url
                (request-parser-http-version parser) version
                (request-parser-buffer parser) nil
                (request-parser-state parser) :header)
          (feed-parser parser rest)))
      (values nil parser nil))))

(defun parse-request-line (line)
  (cl-ppcre:register-groups-bind (method url version)
      (*request-line-regex* line)
    (values method url (or version "0.9"))))

(defun whitespacep (char)
  (member char '(#\space #\tab)))

(defparameter *header-regex* (cl-ppcre:create-scanner "([A-Za-z0-9\\-]+):\\s+(.*)"))

(defun handle-header (parser data)
  (let ((buffer (fill-buffer parser data)))
    (if-let (match (search '(#\return #\linefeed) buffer))
      (progn
        (setf (request-parser-buffer parser) nil)
        (when (eql 0 match)
          (setf (request-parser-state parser) :done)
          (nreversef (request-parser-headers parser))
          (return-from handle-header (feed-parser parser (subseq buffer (+ match 2)))))
        (let ((header-line (subseq buffer 0 match))
              (rest (subseq buffer (+ match 2))))
          (cond ((whitespacep (elt header-line 0))
                 (let ((header-cons (car (request-parser-headers parser))))
                   (setf (cdr header-cons) (concatenate 'string (cdr header-cons)
                                                        " "
                                                        (string-trim '(#\space #\tab) header-line))))
                 (feed-parser parser rest))
                (t
                 (cl-ppcre:register-groups-bind (name value)
                     (*header-regex* header-line)
                   (push (cons (intern (string-upcase name) :keyword) value) (request-parser-headers parser)))
                 (feed-parser parser rest)))))
      (values nil parser nil))))

(defparameter +crlf-ascii+ #.(make-array 2 :element-type 'character :initial-contents '(#\return #\linefeed)))
(defun test ()
  (let ((request-chunk (concatenate 'string
                                    "200 / HTTP/1.1" +crlf-ascii+
                                    "Foo: bar" +crlf-ascii+
                                    " baz" +crlf-ascii+
                                    "Quux: Hurr" +crlf-ascii+
                                    +crlf-ascii+)))
    (feed-parser (make-request-parser) request-chunk)))
