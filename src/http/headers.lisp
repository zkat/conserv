(in-package #:conserv.http)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parser stages support
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mk-combined-symbol (base attached &optional intern-package)
    "creates a new symbol based on base and attached.  the symbol is interned in the same package as base, unless intern-package is non-nil.  if intern-package is non-nil the new symbol is interned in the package which the package designator intern-package shows."
    (intern (concatenate 'string (string base) "-" (string attached))
            (if intern-package intern-package
                (symbol-package base)))))

(defmacro call-parser-stage (name stage parser-state)
  `(,(mk-combined-symbol name stage) ,parser-state))

(defmacro parser-stage (name stage)
  `(function ,(mk-combined-symbol name stage)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-in-tree (tree source target &optional (predicate #'equal))
    (cond ((funcall predicate tree source)
           target)
          ((listp tree)
           (loop for item in tree
              collect (expand-in-tree item source target predicate)))
          (t tree))))

(defmacro define-parser-stages ((parser-state-var) name &rest stages)
  (let ((expanded-stages nil))
    (loop for stage in (reverse stages)
       do (loop for es in expanded-stages ;; does this direction induce continuously expanded stages? (it should be irrelevant, yet still)
             do (setf stage (expand-in-tree stage
                                            `(call-stage ,(first es))
                                            `(progn ,@(rest es)))))
       do (push stage expanded-stages))
    (setf stages expanded-stages))
  (setf stages (mapcar (lambda (stage)
                         (cons (mk-combined-symbol name (first stage))
                               (rest stage)))
                       stages))
  `(progn ;; (declaim (inline ,@(mapcar #'first stages))) ;; inlining is done manually, so we don't recursively expand
     (macrolet ((call-stage (stage)
                  (list (mk-combined-symbol (quote ,name) stage)
                        (quote ,parser-state-var)))
                (stage-func (stage)
                  (list 'function (mk-combined-symbol (quote ,name) stage))))
       ,@(mapcar (lambda (stage)
                   `(defun ,(first stage) (,parser-state-var)
                      (declare (type parser-state ,parser-state-var)
                               (ignorable ,parser-state-var))
                      ,@(rest stage)))
                 stages))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BUFFER IMPLEMENTATION
(defstruct multi-buffer
  "a buffer which simplifies manually walking over strings.  the strings upon which the parsing occurs can be delivered in chunks.  allows the user to easily mark and cut regions and specify a way of calling a specific function if the requested region isn't available yet.  in this way it's fairly simple to handle the postponed receiving of content."
  (string "" :type simple-string) ; This contains the chunk we're currently processing
  (pstrings nil :type list) ; This contains the chunks which haven't fully been parsed yet.
  (index 0 :type fixnum) ; The current index in the buffer.  indicates the index in <string>, and should always be greater than 0.
  (mark 0 :type fixnum)) ; The marked index in the buffer.  if the mark is negative, it indicates that one should walk from the list in pstrings backward (first the first string in <pstrings> is walked from end to start, until the mark is 0).  if the mark is 0 the currently active character is the first character in the current string.

(define-condition buffer-lacks-data (error)
  ((recover-function :reader recover-function :initarg :recover-function :type function))
  (:documentation "error which indicates that the request couldn't be handled, due to a lack of data.  recover-function is the function te be called when more data has become available in the buffer."))

(defun feed-buffer (buffer string)
  "feeds a new string to the buffer"
  (declare (type multi-buffer buffer)
           (type simple-string string))
  (let ((shift (length (multi-buffer-string buffer))))
    (decf (multi-buffer-index buffer) shift)
    (decf (multi-buffer-mark buffer) shift))
  (push (multi-buffer-string buffer) (multi-buffer-pstrings buffer))
  (setf (multi-buffer-string buffer) string))

(declaim (inline previous-buffer-content-for-index previous-buffer-content-from-index char-for-index current-char mark-buffer peek-forward n-peek-forward buffer-forward n-buffer-forward buffer-unused-available-content copy-marked-region forward-buffer-below forward-buffer-while-not))

(declaim (ftype (function (multi-buffer fixnum) simple-string) previous-buffer-content-from-index)
         (ftype (function (multi-buffer fixnum) character) previous-buffer-content-for-index) 
         (ftype (function (multi-buffer fixnum function) character) char-for-index)
         (ftype (function (multi-buffer function) character) current-char peek-forward)
         (ftype (function (multi-buffer fixnum function) character) n-peek-forward)
         (ftype (function (multi-buffer)) mark-buffer buffer-forward buffer-unused-available-content)
         (ftype (function (multi-buffer fixnum)) n-buffer-forward)
         (ftype (function (multi-buffer function) simple-string) copy-marked-region))

(defun previous-buffer-content-for-index (buffer index)
  "returns the content of <buffer's> negative index <index>.  this indicates that we must walk <-n> instances in the history of the buffer and yield the instance at that place."
  (declare (type multi-buffer buffer)
           (type fixnum index))
  (setf index (- index))
  (or
   (loop named listwalker
      for (the simple-string string) in (multi-buffer-pstrings buffer)
      for stringlength = (length (the simple-string string))
      if (> index stringlength)
      do (decf index stringlength)
      else
      do (return-from listwalker (elt string (- stringlength index))))
   (error "previous-buffer-content-for-index was called for impossible index")))

(defun previous-buffer-content-from-index (buffer index)
  "returns the content from <buffer>'s negative index <index>.  this indicates that we must walk <-n> characters in the history of the buffer and yield the content up to the present contents.  this does *not* include any part of the currently active string in the buffer."
  (declare (type multi-buffer buffer)
           (type fixnum index))
  (let ((lists-to-append nil)
        (leftover-index (- index)))
    (declare (type list lists-to-append)
             (type fixnum leftover-index))
    (loop named listwalker
       for string of-type simple-string in (multi-buffer-pstrings buffer)
       for stringlength = (length (the simple-string string))
       if (> stringlength leftover-index)
       do (progn (push leftover-index lists-to-append)
             (decf leftover-index stringlength))
       else
       do (return-from listwalker
            (apply #'concatenate 'string (nreverse (cons (subseq string (- stringlength leftover-index))
                                                         (nreverse lists-to-append))))))))

(defun char-for-index (buffer index callback)
  "yields the character of <buffer> at <index>"
  (declare (type multi-buffer buffer)
           (type fixnum index)
           (type function callback))
  (cond ((< index 0)
         (previous-buffer-content-for-index buffer index))
        ((< index (length (multi-buffer-string buffer)))
         (elt (multi-buffer-string buffer) index))
        ((error 'buffer-lacks-data :recover-function callback))))

(defun current-char (buffer callback)
  "yields the character currently under the buffer"
  (declare (type multi-buffer buffer)
           (type function callback))
  (char-for-index buffer (multi-buffer-index buffer) callback))

(defun mark-buffer (buffer)
  "sets the mark of the buffer to the current index"
  (declare (type multi-buffer buffer))
  (setf (multi-buffer-mark buffer) (multi-buffer-index buffer)))

(defun peek-forward (buffer callback)
  "shows the first character after the current index, without moving the index"
  (declare (type multi-buffer buffer)
           (type function callback))
  (char-for-index buffer (1+ (multi-buffer-index buffer)) callback))

(defun n-peek-forward (buffer n callback)
  "shows the character <n> characters after the current index, without moving the index"
  (declare (type multi-buffer buffer)
           (type fixnum n)
           (type function callback))
  (char-for-index buffer (+ n (multi-buffer-index buffer)) callback))

(defun buffer-forward (buffer)
  "moves the buffer's forward by one"
  (declare (type multi-buffer buffer))
  (incf (multi-buffer-index buffer)))

(defun n-buffer-forward (buffer n)
  "moves the buffer n characters forward"
  (declare (type multi-buffer buffer)
           (type fixnum n))
  (incf (multi-buffer-index buffer) n))

(defun buffer-unused-available-content (buffer)
  "yields all content in the buffer which is after the current index"
  (declare (type multi-buffer buffer))
  (if (<= (multi-buffer-index buffer) 0)
      (concatenate 'string (previous-buffer-content-from-index buffer (multi-buffer-index buffer)))
      (subseq (multi-buffer-string buffer) (multi-buffer-index buffer))))

(defun copy-marked-region (buffer callback)
  "copies the region up to (and including) the mark and the current index into a new sequence"
  (declare (type multi-buffer buffer)
           (type function callback))
  (when (>= (multi-buffer-index buffer) (length (multi-buffer-string buffer)))
    (error 'buffer-lacks-data :recover-function callback))
  (cond ((and (>= (multi-buffer-mark buffer) 0)
            ) ; (>= 0 (multi-buffer-index buffer)) ;; implicitly true, the index is always further than the mark when calling this
         (subseq (multi-buffer-string buffer)
                 (multi-buffer-mark buffer)
                 (1+ (multi-buffer-index buffer))))
        ((and (< (multi-buffer-mark buffer) 0)
            (>= (multi-buffer-index buffer) 0))
         (concatenate 'string
                      (previous-buffer-content-from-index buffer (multi-buffer-mark buffer))
                      (subseq (multi-buffer-string buffer) 0 (1+ (multi-buffer-index buffer)))))
        (T ; both are negative
         (let ((upto-mark (previous-buffer-content-from-index buffer (multi-buffer-mark buffer))))
           (declare (type simple-string upto-mark))
           (subseq upto-mark 0 (+ (length upto-mark) 1 (multi-buffer-index buffer)))))))

(defun forward-buffer-below (buffer &key elements not-followed-by restart-callback)
  "forwards the buffer until the index is at a position after which each of the values of <elements> are listed in sequence, followed by neither character of <not-followed-by>."
  (declare (type multi-buffer buffer)
           (type list elements)
           (type list not-followed-by)
           (type function restart-callback))
  (let ((bound (1+ (length elements))))
    (loop until (and (block elements-correct-p
                  (loop for n of-type fixnum from 1
                     for element of-type character in elements
                     unless (eql (n-peek-forward buffer n restart-callback)
                                 element)
                     do (return-from elements-correct-p nil))
                  T)
                (not (find (n-peek-forward buffer bound restart-callback)
                         not-followed-by
                         :test (lambda (a b)
                                 (declare (type character a b))
                                 (eql a b)))))
       do (buffer-forward buffer))))

(defun forward-buffer-while-not (buffer char callback)
  "forwards the buffer until the character right after the <buffer>'s index is <character>"
  (declare (type multi-buffer buffer)
           (type character char)
           (type function callback))
  (loop until (eql char (peek-forward buffer callback))
     do (buffer-forward buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parser and header administration
(defstruct header-info
  "contains all currently parsed information from the header."
  (request-type nil :type symbol) ; type of the request, one of '(:get :head :post :put or :delete)
  (path "" :type simple-string)
  (http-version nil :type symbol) ; version of the http request, one of '(:http-1.0 :http-1.1)
  (headers nil :type list))

(defstruct parser-state
  "A parser for the state itself"
  (buffer (make-multi-buffer) :type multi-buffer)
  (header-info (make-header-info) :type header-info)
  (parser-stage-function (parser-stage parse-request-line :init)))

(defun make-request-parser ()
  (make-parser-state))

(defun feed-parser (parser-state string)
  "gives <parser-state> a new string of data, which it will then parse and yield the results of"
  (feed-buffer (parser-state-buffer parser-state)
               (if (typep string 'simple-string)
                   string
                   (coerce string 'simple-string)))
  (handler-case (funcall (parser-state-parser-stage-function parser-state) parser-state)
    (buffer-lacks-data (bld)
      (setf (parser-state-parser-stage-function parser-state)
            (recover-function bld))
      (values nil parser-state nil))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; parsing the header
(define-parser-stages (parser-state)
    parse-request-line
  (:init (case (char-upcase (current-char (parser-state-buffer parser-state) ;; this matching is fast, but it isn't pretty when things go haywire.
                                          (stage-func :init)))
           (#\P (buffer-forward (parser-state-buffer parser-state))
                (call-stage :post-or-put))
           (#\H (setf (header-info-request-type (parser-state-header-info parser-state))
                      :head)
                (n-buffer-forward (parser-state-buffer parser-state) #.(1+ (length "HEAD"))))
           (#\G (setf (header-info-request-type (parser-state-header-info parser-state))
                      :get)
                (n-buffer-forward (parser-state-buffer parser-state) #.(1+ (length "GET"))))
           (#\D (setf (header-info-request-type (parser-state-header-info parser-state))
                      :delete)
                (n-buffer-forward (parser-state-buffer parser-state) #.(1+ (length "DELETE"))))
           (T (error "i don't know this request type. HEAD GET DELETE POST and PUT are supported.")))
         (call-stage :request-path))
  (:post-or-put (case (char-upcase (current-char (parser-state-buffer parser-state)
                                                 (stage-func :post-or-put)))
                  (#\O (n-buffer-forward (parser-state-buffer parser-state) #.(length "POST")))
                  (#\U (n-buffer-forward (parser-state-buffer parser-state) #.(length "PUT")))
                  (T (error "i don't know this request type. HEAD GET DELETE POST and PUT are supported.")))
                (call-stage :request-path))
  (:request-path ;; once the request-path is entered, the buffer's current character must be the first character of the path
   (call-stage :request-path-init)
   (call-stage :request-path-discover))
  (:request-path-init (mark-buffer (parser-state-buffer parser-state)))
  (:request-path-discover (forward-buffer-while-not (parser-state-buffer parser-state) #\Space
                                                    (stage-func :request-path-discover))
                          (setf (header-info-path (parser-state-header-info parser-state))
                                (copy-marked-region (parser-state-buffer parser-state)
                                                    (stage-func :error))) ;; we have just discovered up to the character /after/ us, so we can't yield an error
                          (n-buffer-forward (parser-state-buffer parser-state) 2)
                          (call-stage :http-discover-version))
  (:http-discover-version (n-buffer-forward (parser-state-buffer parser-state) #.(length "HTTP/1."))
                          (call-stage :http-discover-version-nr))
  (:http-discover-version-nr (case (current-char (parser-state-buffer parser-state)
                                                 (stage-func :http-discover-version-nr))
                               (#\0 (setf (header-info-http-version (parser-state-header-info parser-state))
                                          :http-1.0))
                               (#\1 (setf (header-info-http-version (parser-state-header-info parser-state))
                                          :http-1.1))
                               (T (error "HTTP version header not understood, it returned ~A" (current-char (parser-state-buffer parser-state) (stage-func :error)))))
                             (n-buffer-forward (parser-state-buffer parser-state)
                                               #. (1+ (length (list #\Return #\Linefeed))))
                             (call-parser-stage parse-header-lines :init  parser-state))
  (:error (error "the header parser made an error.  please reggister the request and send a bug-report")))

(define-parser-stages (parser-state)
    parse-header-lines
  (:init (mark-buffer (parser-state-buffer parser-state))
         (if (and (eql #\Return (current-char (parser-state-buffer parser-state) (stage-func :init)))
                (eql #\Newline (peek-forward (parser-state-buffer parser-state) (stage-func :init))))
             (progn (n-buffer-forward (parser-state-buffer parser-state) 2)
                (call-stage :end-header-parsing))
             (progn (mark-buffer (parser-state-buffer parser-state))
                (call-stage :copy-header-keyword))))
  (:copy-header-keyword (forward-buffer-while-not (parser-state-buffer parser-state) #\:
                                                  (stage-func :copy-header-keyword))
                        (push (cons (string-upcase (copy-marked-region (parser-state-buffer parser-state)
                                                                       (stage-func :error))) ; we can't reach this, forward-buffer-while-not must have read all parts of the buffer we've used so far
                                    nil)
                              (header-info-headers (parser-state-header-info parser-state)))
                        (call-stage :read-header-value-init))
  (:read-header-value-init (buffer-forward (parser-state-buffer parser-state)) ; forward the #\:
                           (mark-buffer (parser-state-buffer parser-state))
                           (call-stage :read-header-value))
  (:read-header-value (forward-buffer-below (parser-state-buffer parser-state)
                                            :elements (list #\Return #\Linefeed)
                                            :not-followed-by (list #\Space #\Tab)
                                            :restart-callback (stage-func :read-header-value))
                      (setf (cdr (first (header-info-headers (parser-state-header-info parser-state))))
                            (copy-marked-region (parser-state-buffer parser-state)
                                                (stage-func :error)))
                      (n-buffer-forward (parser-state-buffer parser-state)
                                        3)
                      (call-stage :init))
  (:end-header-parsing (values T
                               (parser-state-header-info parser-state)
                               (buffer-unused-available-content (parser-state-buffer parser-state))))
  (:error (error "something odd, impossible happened whilst parsing the header.  this is probably due to a bug in the implementation of the header parsing.")))

;;;;;;;;;;;;;;;;;;
;;;; previous code
;;;;;;;;;;;;;;;;;;




;; (defstruct request-parser
;;   buffer
;;   (state :request-line)
;;   method
;;   url
;;   http-version
;;   headers)

;; (define-condition request-parser-error (error) ())

;; (defun feed-parser (parser data)
;;   (case (request-parser-state parser)
;;     (:request-line
;;      (handle-request-line parser data))
;;     (:header
;;      (handle-header parser data))
;;     (:done
;;      (values t parser data))))

;; (defun fill-buffer (parser data)
;;   (setf (request-parser-buffer parser)
;;         (if-let (buffer (request-parser-buffer parser))
;;           (concatenate 'string buffer data)
;;           data)))

;; (defparameter *request-line-regex*
;;   (cl-ppcre:create-scanner "(\\w+)\\s+([^\\s]+)\\s+(?:HTTP/(\\d\\.\\d))?"
;;                            :case-insensitive-mode t))

;; (defun handle-request-line (parser data)
;;   (let ((buffer (fill-buffer parser data)))
;;     (if-let (match (search '(#\return #\linefeed) buffer))
;;       (let ((request-line (subseq buffer 0 match))
;;             (rest (subseq buffer (+ match 2))))
;;         (multiple-value-bind (method url version)
;;             (parse-request-line request-line)
;;           (setf (request-parser-method parser) method
;;                 (request-parser-url parser) url
;;                 (request-parser-http-version parser) version
;;                 (request-parser-buffer parser) nil
;;                 (request-parser-state parser) :header)
;;           (feed-parser parser rest)))
;;       (values nil parser nil))))

;; (defun parse-request-line (line)
;;   (cl-ppcre:register-groups-bind (method url version)
;;       (*request-line-regex* line)
;;     (values method url (or version "0.9"))))

;; (defun whitespacep (char)
;;   (member char '(#\space #\tab)))

;; (defparameter *header-regex* (cl-ppcre:create-scanner "([A-Za-z0-9\\-]+):\\s+(.*)"))

;; (defun handle-header (parser data)
;;   (let ((buffer (fill-buffer parser data)))
;;     (if-let (match (search '(#\return #\linefeed) buffer))
;;       (progn
;;         (setf (request-parser-buffer parser) nil)
;;         (when (eql 0 match)
;;           (setf (request-parser-state parser) :done)
;;           (nreversef (request-parser-headers parser))
;;           (return-from handle-header (feed-parser parser (subseq buffer (+ match 2)))))
;;         (let ((header-line (subseq buffer 0 match))
;;               (rest (subseq buffer (+ match 2))))
;;           (cond ((whitespacep (elt header-line 0))
;;                  (let ((header-cons (car (request-parser-headers parser))))
;;                    (setf (cdr header-cons) (concatenate 'string (cdr header-cons)
;;                                                         " "
;;                                                         (string-trim '(#\space #\tab) header-line))))
;;                  (feed-parser parser rest))
;;                 (t
;;                  (cl-ppcre:register-groups-bind (name value)
;;                      (*header-regex* header-line)
;;                    (push (cons (intern (string-upcase name) :keyword) value) (request-parser-headers parser)))
;;                  (feed-parser parser rest)))))
;;       (values nil parser nil))))

(defparameter +crlf-ascii+ #.(make-array 2 :element-type 'character :initial-contents '(#\return #\linefeed)))
(defun test ()
  (let ((request-chunk (concatenate 'string
                                    "GET / HTTP/1.1" +crlf-ascii+
                                    "Foo: bar" +crlf-ascii+
                                    " baz" +crlf-ascii+
                                    "Quux: Hurr" +crlf-ascii+
                                    +crlf-ascii+)))
    (feed-parser (make-request-parser) request-chunk)))

(declaim (ftype (function (header-info string-designator) (or null simple-string)) request-header))
(defun request-header (header-info header)
  (let ((match (find (string header) (header-info-headers header-info) :key #'car :test #'string=)))
    (when match (cdr match))))

;; methods for http.lisp
(defun request-parser-method (header-info)
  (header-info-request-type header-info))
(defun request-parser-http-version (header-info)
  (header-info-http-version header-info))
(defun request-parser-url (header-info)
  (header-info-path header-info))
