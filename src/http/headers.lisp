(in-package #:conserv.http)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;;;;;;;;
;;;; types
(deftype ascii-char ()
  '#.(if (and (every
             (lambda (char) (typep char 'base-char))
             (concatenate 'string
                          #(#\Backspace #\Linefeed #\Newline #\Page #\Return 	#\Rubout 	#\Space 	#\Tab)
                          "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~")))
         'base-char 'character))

(deftype ascii-simple-string ()
  '(simple-array ascii-char))

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

(defmacro define-parser-stages ((parser-state-var header-info-var buffer-var) name &rest stages)
  #+conserv.http-break-on-entering-parser-stage
  (setf stages
        (loop for stage in stages
           collect `(,(first stage)
                      (break "entering stage ~A" ,(first stage))
                      ,@(rest stage))))
  #-conserv.http-no-inline-parser-stages
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
                      (let ((,header-info-var (parser-state-header-info ,parser-state-var))
                            (,buffer-var (parser-state-buffer ,parser-state-var)))
                        (declare (ignorable ,header-info-var ,buffer-var)
                                 (type header-info ,header-info-var)
                                 (type multi-buffer ,buffer-var))
                       ,@(rest stage))))
                 stages))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BUFFER IMPLEMENTATION
(defstruct multi-buffer
  "a buffer which simplifies manually walking over strings.  the strings upon which the parsing occurs can be delivered in chunks.  allows the user to easily mark and cut regions and specify a way of calling a specific function if the requested region isn't available yet.  in this way it's fairly simple to handle the postponed receiving of content."
  (string "" :type ascii-simple-string) ; This contains the chunk we're currently processing
  (pstrings nil :type list) ; This contains the chunks which haven't fully been parsed yet.
  (index 0 :type fixnum) ; The current index in the buffer.  indicates the index in <string>, and should always be greater than 0.
  (mark 0 :type fixnum)) ; The marked index in the buffer.  if the mark is negative, it indicates that one should walk from the list in pstrings backward (first the first string in <pstrings> is walked from end to start, until the mark is 0).  if the mark is 0 the currently active character is the first character in the current string.

(define-condition buffer-lacks-data (error)
  ((recover-function :reader recover-function :initarg :recover-function :type function))
  (:documentation "error which indicates that the request couldn't be handled, due to a lack of data.  recover-function is the function te be called when more data has become available in the buffer."))

(defun feed-buffer (buffer string)
  "feeds a new string to the buffer"
  (declare (type multi-buffer buffer)
           (type ascii-simple-string string))
  (let ((shift (length (multi-buffer-string buffer))))
    (decf (multi-buffer-index buffer) shift)
    (decf (multi-buffer-mark buffer) shift))
  (push (multi-buffer-string buffer) (multi-buffer-pstrings buffer))
  (setf (multi-buffer-string buffer) string))

#-conserv.http-no-inline-buffer-operations
(declaim (inline previous-buffer-content-for-index previous-buffer-content-from-index char-for-index current-char mark-buffer peek-forward n-peek-forward buffer-forward n-buffer-forward n-buffer-backward buffer-unused-available-content copy-marked-region forward-buffer-below forward-buffer-while-not forward-buffer-while next-char-blank-p buffer-at-blank-p forward-until-before-clrf skip-all-blanks buffer-at-crlf-p forward-to-before-space))

(declaim (ftype (function (multi-buffer fixnum) ascii-simple-string) previous-buffer-content-from-index)
         (ftype (function (multi-buffer fixnum) ascii-char) previous-buffer-content-for-index) 
         (ftype (function (multi-buffer fixnum function) ascii-char) char-for-index)
         (ftype (function (multi-buffer function) ascii-char) current-char peek-forward)
         (ftype (function (multi-buffer fixnum function) ascii-char) n-peek-forward)
         (ftype (function (multi-buffer)) mark-buffer buffer-forward)
         (ftype (function (multi-buffer) ascii-simple-string) buffer-unused-available-content)
         (ftype (function (multi-buffer fixnum)) n-buffer-forward)
         (ftype (function (multi-buffer function) ascii-simple-string) copy-marked-region))

(defun previous-buffer-content-for-index (buffer index)
  "returns the content of <buffer's> negative index <index>.  this indicates that we must walk <-n> instances in the history of the buffer and yield the instance at that place."
  (declare (type multi-buffer buffer)
           (type fixnum index))
  (setf index (- index))
  (or
   (loop named listwalker
      for (the ascii-simple-string string) in (multi-buffer-pstrings buffer)
      for stringlength = (length (the ascii-simple-string string))
      if (> index stringlength)
      do (decf index stringlength)
      else
      do (return-from listwalker (elt string (- stringlength index))))
   (error "previous-buffer-content-for-index was called for impossible index")))

(defmacro efficient-sum-lengths (list-var object-type)
  (let ((sum (gensym "sum"))
        (item (gensym "item")))
    `(let ((,sum 0))
       (declare (type fixnum ,sum))
       (dolist (,item ,list-var)
         (declare (type ,object-type ,item))
         (incf ,sum (the fixnum (length ,item))))
       ,sum)))

(defun append-ascii-strings (&rest simple-strings)
  "appends a series of simple-strings"
  (declare (type list simple-strings))
  (if simple-strings
      (let ((ascii-simple-string (make-array (efficient-sum-lengths simple-strings ascii-simple-string)
                                             :element-type 'ascii-char
                                             :initial-element #\Space))
            (start-index 0))
        (declare (type ascii-simple-string ascii-simple-string)
                 (type fixnum start-index))
        (loop for ass in simple-strings
           do (replace ascii-simple-string (the ascii-simple-string ass)
                       :start1 start-index)
             (incf start-index (length (the ascii-simple-string ascii-simple-string))))
        ascii-simple-string)
      (make-array 0 :element-type 'ascii-char)))

(defun make-ascii-simple-string (string)
  "creates an ascii-simple-string from a string"
  (declare (type string string))
  (make-array (length string)
              :element-type 'ascii-char
              :initial-contents string))

(defun previous-buffer-content-from-index (buffer index)
  "returns the content from <buffer>'s negative index <index>.  this indicates that we must walk <-n> characters in the history of the buffer and yield the content up to the present contents.  this does *not* include any part of the currently active string in the buffer."
  (declare (type multi-buffer buffer)
           (type fixnum index))
  (let ((lists-to-append nil)
        (leftover-index (- index)))
    (declare (type list lists-to-append)
             (type fixnum leftover-index))
    (loop for string of-type ascii-simple-string in (multi-buffer-pstrings buffer)
       for stringlength = (length (the ascii-simple-string string))
       if (> stringlength leftover-index)
       do (progn (push leftover-index lists-to-append)
             (decf leftover-index stringlength))
       else
       do (return-from previous-buffer-content-from-index
            (the ascii-simple-string
              (apply #'append-ascii-strings (nreverse (cons (subseq string (- stringlength leftover-index))
                                                            (nreverse lists-to-append))))))))
  (make-ascii-simple-string "")) ; here to make sbcl's type system happy (case shouldn't occur in practice)

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

(defun n-buffer-backward (buffer n)
  "moves the buffer n characters backward"
  (declare (type multi-buffer buffer)
           (type fixnum n))
  (decf (multi-buffer-index buffer) n))

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
         (the ascii-simple-string
           (subseq (multi-buffer-string buffer)
                   (multi-buffer-mark buffer)
                   (1+ (multi-buffer-index buffer)))))
        ((and (< (multi-buffer-mark buffer) 0)
            (>= (multi-buffer-index buffer) 0))
         (the ascii-simple-string
           (append-ascii-strings (previous-buffer-content-from-index buffer (multi-buffer-mark buffer))
                                 (subseq (multi-buffer-string buffer) 0 (1+ (multi-buffer-index buffer))))))
        (T ; both are negative
         (let ((upto-mark (previous-buffer-content-from-index buffer (multi-buffer-mark buffer))))
           (declare (type ascii-simple-string upto-mark))
           (the ascii-simple-string
             (subseq upto-mark 0 (+ (length upto-mark) 1 (multi-buffer-index buffer))))))))

(defun forward-buffer-below (buffer &key elements not-followed-by restart-callback)
  "forwards the buffer until the index is at a position after which each of the values of <elements> are listed in sequence, followed by neither character of <not-followed-by>."
  (declare (type multi-buffer buffer)
           (type ascii-simple-string elements)
           (type ascii-simple-string not-followed-by)
           (type function restart-callback))
  (let ((bound (1+ (length elements))))
    (loop until (and (block elements-correct-p
                  (loop for n of-type fixnum from 1
                     for element of-type character across elements
                     unless (eql (n-peek-forward buffer n restart-callback)
                                 element)
                     do (return-from elements-correct-p nil))
                  T)
                (not (find (n-peek-forward buffer bound restart-callback)
                         not-followed-by
                         :test #'eql)))
       do (buffer-forward buffer))))

(defun forward-buffer-while-not (buffer char callback)
  "forwards the buffer until the character right after the <buffer>'s index is <character>"
  (declare (type multi-buffer buffer)
           (type character char)
           (type function callback))
  (loop until (eql char (peek-forward buffer callback))
     do (buffer-forward buffer)))

(defun forward-buffer-while (buffer characters callback)
  "forwards the buffer until the character right on the <buffer>'s index is not one of those in characters"
  (declare (type multi-buffer buffer)
           (type ascii-simple-string characters)
           (type function callback))
  (loop while (find (current-char buffer callback) characters :test #'eql)
     do (buffer-forward buffer)))

(defun forward-to-before-space (buffer callback)
  "forwards the buffer until the character right after the <buffer>'s index is a space"
  (declare (type multi-buffer buffer)
           (type function callback))
  (loop until (eql #\Space (peek-forward buffer callback))
     do (buffer-forward buffer)))

(defun buffer-at-crlf-p (buffer callback)
  "returns non-nil iff the buffer's currently on #\Return and the next character is #\Newline"
  (declare (type multi-buffer buffer)
           (type function callback))
  (and (eql #\Return (current-char buffer callback))
     (eql #\Newline (peek-forward buffer callback))))

(defun skip-all-blanks (buffer callback)
  "forwards the buffer's index until it's not at a blank character anymore.  blank characters are #\Space #\Tab"
  (declare (type multi-buffer buffer)
           (type function callback))
  (loop for current = (current-char buffer callback)
     while (or (eql current #\Space)
              (eql current #\Tab))
     do (buffer-forward buffer)))

(defun forward-until-before-clrf (buffer callback)
  "forwards the buffer's index until the characters right after the index are #\Return #\Newline"
  (declare (type multi-buffer buffer)
           (type function callback))
  (loop until (and (eql (peek-forward buffer callback) #\Return)
              (eql (n-peek-forward buffer 2 callback) #\Newline))
     do (buffer-forward buffer)))

(defun buffer-at-blank-p (buffer callback)
  "returns non-nil iff the current character of the buffer is either #\Space or #\Tab"
  (declare (type multi-buffer buffer)
           (type function callback))
  (let ((current-char (current-char buffer callback)))
    (or (eql current-char #\Space)
       (eql current-char #\Tab))))

(defun next-char-blank-p (buffer callback)
  "returns non-nil iff the character after the buffer's index is either #\Space or #\Tab"
  (declare (type multi-buffer buffer)
           (type function callback))
  (let ((peek (peek-forward buffer callback)))
    (or (eql peek #\Space)
       (eql peek #\Tab))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; parser and header administration
(defstruct header-info
  "contains all currently parsed information from the header."
  (request-type nil :type symbol) ; type of the request, one of '(:get :head :post :put or :delete)
  (path "" :type ascii-simple-string)
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
               (if (typep string 'ascii-simple-string)
                   string
                   (make-ascii-simple-string string)))
  (handler-case (funcall (parser-state-parser-stage-function parser-state) parser-state)
    (buffer-lacks-data (bld)
      (setf (parser-state-parser-stage-function parser-state)
            (recover-function bld))
      (values nil parser-state nil))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; parsing the header
(define-parser-stages (parser-state header-info buffer)
    parse-request-line
  (:init (case (char-upcase (current-char buffer ;; this matching is fast, but it isn't pretty when things go haywire.
                                          (stage-func :init)))
           (#\P (buffer-forward buffer)
                (call-stage :post-or-put))
           (#\H (setf (header-info-request-type header-info)
                      :head)
                (n-buffer-forward buffer #.(1+ (length "HEAD"))))
           (#\G (setf (header-info-request-type header-info)
                      :get)
                (n-buffer-forward buffer #.(1+ (length "GET"))))
           (#\C (setf (header-info-request-type header-info)
                      :connect)
                (n-buffer-forward buffer #.(1+ (length "CONNECT"))))
           (#\D (setf (header-info-request-type header-info)
                      :delete)
                (n-buffer-forward buffer #.(1+ (length "DELETE"))))
           (T (error "i don't know this request type. HEAD GET DELETE POST and PUT are supported.")))
         (call-stage :request-path))
  (:post-or-put (case (char-upcase (current-char buffer
                                                 (stage-func :post-or-put)))
                  (#\O (n-buffer-forward buffer #.(length "POST")))
                  (#\U (n-buffer-forward buffer #.(length "PUT")))
                  (T (error "i don't know this request type. HEAD GET DELETE POST and PUT are supported.")))
                (call-stage :request-path))
  (:request-path ;; once the request-path is entered, the buffer's current character must be the first character of the path
   (call-stage :request-path-init)
   (call-stage :request-path-discover))
  (:request-path-init (mark-buffer buffer))
  (:request-path-discover (forward-to-before-space buffer (stage-func :request-path-discover))
                          (setf (header-info-path header-info)
                                (copy-marked-region buffer
                                                    (stage-func :error))) ;; we have just discovered up to the character /after/ us, so we can't yield an error
                          (n-buffer-forward buffer 2)
                          (call-stage :http-discover-version))
  (:http-discover-version (n-buffer-forward buffer #.(length "HTTP/1."))
                          (call-stage :http-discover-version-nr))
  (:http-discover-version-nr (setf (header-info-http-version header-info)
                                   (case (current-char buffer
                                                       (stage-func :http-discover-version-nr))
                                     (#\0 :http-1.0)
                                     (#\1 :http-1.1)
                                     (T (error "HTTP version header not understood, it returned ~A" (current-char buffer (stage-func :error))))))
                             (n-buffer-forward buffer
                                               #.(1+ (length (list #\Return #\Linefeed))))
                             (call-parser-stage parse-header-lines :init parser-state))
  (:error (error "the header parser made an error.  either you sent a header we don't understand, or you should register the request and send a bug-report")))

(define-parser-stages (parser-state header-info buffer)
    parse-header-lines
  (:init (mark-buffer buffer)
         (if (buffer-at-crlf-p buffer (stage-func :init))
             (progn (n-buffer-forward buffer 2)
                (call-stage :end-header-parsing))
             (progn (mark-buffer buffer)
                (call-stage :copy-header-keyword))))
  (:copy-header-keyword (forward-buffer-while-not buffer #\:
                                                  (stage-func :copy-header-keyword))
                        (push (cons (string-upcase (copy-marked-region buffer
                                                                       (stage-func :error))) ; we can't reach this, forward-buffer-while-not must have read all parts of the buffer we've used so far
                                    nil)
                              (header-info-headers header-info))
                        (n-buffer-forward buffer 2)
                        (call-stage :skip-header-value-spaces))
  (:skip-header-value-spaces (skip-all-blanks buffer (stage-func :finish-reading-first-header-value))
                             ;; we're now exactly at the first character
                             (call-stage :read-header-value-init))
  (:read-header-value-init (mark-buffer buffer)
                           (call-stage :read-first-header-value))
  (:read-first-header-value (forward-until-before-clrf buffer (stage-func :read-first-header-value))
                            (n-buffer-forward buffer 3)
                            (call-stage :finish-reading-first-header-value))
  (:finish-reading-first-header-value (if (buffer-at-blank-p buffer (stage-func :finish-reading-first-header-value))
                                          (progn (n-buffer-backward buffer 3)
                                             (push (copy-marked-region buffer
                                                                       (stage-func :error)) ; we peeked further than this, we know what we can copy
                                                   (cdr (first (header-info-headers header-info))))
                                             (n-buffer-forward buffer 3)
                                             (call-stage :read-rest-header-value-init))
                                          (progn (n-buffer-backward buffer 3)
                                             (setf (cdr (first (header-info-headers header-info)))
                                                   (copy-marked-region buffer (stage-func :error))) ; we peeked further than this, we know what we can copy
                                             (n-buffer-forward buffer 3)
                                             (call-stage :init))))
  (:read-rest-header-value-init (skip-all-blanks buffer (stage-func :read-rest-header-value-init))
                                (mark-buffer buffer)
                                (call-stage :read-rest-header-values))
  (:read-rest-header-values (forward-until-before-clrf buffer (stage-func :read-rest-header-values))
                            (call-stage :finish-reading-rest-header-value))
  (:finish-reading-rest-header-value (flet ((push-n-forward ()
                                              (push (copy-marked-region buffer
                                                                        (stage-func :error))
                                                    (cdr (first (header-info-headers header-info))))
                                              (n-buffer-forward buffer 3)))
                                       (if (next-char-blank-p buffer (stage-func :finish-reading-first-header-value))
                                           (progn (push-n-forward)
                                              (call-stage :read-rest-header-value-init))
                                           (progn (push-n-forward)
                                              (setf (cdr (first (header-info-headers header-info)))
                                                    (apply #'concatenate 'string (nreverse (cdr (first (header-info-headers header-info))))))
                                              (call-stage :init)))))
  (:end-header-parsing (values T
                               header-info
                               (buffer-unused-available-content buffer)))
  (:error (error "something odd, impossible happened whilst parsing the header.  this is probably due to a bug in the implementation of the header parsing.")))

(defparameter +crlf-ascii+ #.(make-array 2 :element-type 'character :initial-contents '(#\return #\linefeed)))
(defun test ()
  (let ((request-chunk (concatenate 'string
                                    "GET / HTTP/1.1" +crlf-ascii+
                                    "Foo: bar" +crlf-ascii+
                                    " baz" +crlf-ascii+
                                    "Quux: Hurr" +crlf-ascii+
                                    +crlf-ascii+)))
    (feed-parser (make-request-parser) request-chunk)))

(declaim (ftype (function (list string-designator) (or null simple-string)) request-header))
(defun request-header (header-info-headers header)
  (let ((match (find (string header) header-info-headers :key #'car :test #'string=)))
    (when match (cdr match))))

;; methods for http.lisp
(defun request-parser-method (header-info)
  (header-info-request-type header-info))
(defun request-parser-http-version (header-info)
  (header-info-http-version header-info))
(defun request-parser-url (header-info)
  (header-info-path header-info))
(defun request-parser-headers (header-info)
  (header-info-headers header-info))