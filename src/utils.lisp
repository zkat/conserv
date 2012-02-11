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

(defmacro defprotocol (name nil genfun-specs &rest protocol-options)
  "syntax: (defprotocol <name> ()
             ((<genfun-name> <lambda-list>|:reader|:writer|:accessor
               [:default-form <default-form>]
               [:documentation <docstring>])*)
             (<option-name> . <option-args>*)*

   where

   <name> - Symbol name of the protocol, unevaluated.
   <genfun-name> - A valid function name to be given to the generic function.
   <lambda-list> - A valid generic function lambda list.
   <default-form> - Any valid lisp form.
   <docstring> - A string.
   <option-name> - One of :prefix or :documentation.
   <option-args> - Expected arguments for the given option.

DEFPROTOCOL defines a protocol (not currently a first-class object), which is, logically speaking, a
collection of generic functions that define an expected interface that must be implemented through
DEFMETHOD in order for behavior on a set of objects to be considered complete. On top of grouping
generic function definitions logically, the DEFPROTOCOL facility provides some extra
conveniences. For example, it concatenates the protocol's name to all the <genfun-name>s by default,
similar to the DEFSTRUCT facility.

Generic functions are created using the arguments for each genfun-spec. If :reader, :writer,
or :accessor are provided, simple generic functions meant to be implemented by
DEFCLASS' :reader/:writer/:accessor are created. Otherwise, if a lambda list if provided, that list
will be used as the generic function's lambda list.

The :default-form argument, if provided, defines a default method to the generic function with the
provided form as its body. For genfun specs with lambda-lists, the variables defined in the lambda
list are made available to the form. In the case of :reader/:writer/:accessor, no such variable is
made available. example: (frob (thing) :default-form (error \"Function not implemented for object
of class ~S\" (class-of thing)))

The :prefix option is used to control name concatenation. By default, DEFPROTOCOL concatenates the
protocol's name to the name of each generic function (or, in the case of SETF functions, to the
second item in the function name). The single argument to :prefix should be either a
literal (unevaluated) symbol, or NIL. If NIL is provided, no concatenation at all will be done by
DEFPROTOCOL.

Finally, the :documentation argument for both the DEFPROTOCOL form and genfun specs are simply meant to
provide useful documentation to the next victim of your code. In the case of genfun-specs,
the :documentation argument is used as the :documentation argument to the defgeneric form.
"
  (loop for (option-name . nil) in protocol-options
     unless (member option-name '(:documentation :prefix))
     do (error "Unrecognized defprotocol option: ~S" option-name))
  (let* ((prefix-cons (assoc :prefix protocol-options))
         (prefix (cond ((and prefix-cons (cadr prefix-cons))
                        (string (cadr prefix-cons)))
                       ((and prefix-cons (null (cadr prefix-cons)))
                        nil)
                       ((null prefix-cons)
                        (concatenate 'string (string name) "-"))
                       (t (error "Missed a case in defprotocol?")))))
    `(progn
       ,@(loop for spec in genfun-specs
            collect
            (destructuring-bind (name type-or-lambda-list &key documentation (default-form nil default-form-p))
                spec
              (unless (or (symbolp name)
                          (and (listp name)
                               (eq 'setf (car name))
                               (symbolp (cadr name))))
                (error "Invalid function name: ~S" name))
              (when prefix
                (setf name
                      (if (listp name)
                          (list 'setf (intern (concatenate 'string prefix (string (cadr name)))))
                          (intern (concatenate 'string prefix (string name))))))
              (etypecase type-or-lambda-list
                (list
                 `(defgeneric ,name ,type-or-lambda-list
                    ,@(when documentation `((:documentation ,documentation)))
                    ,@(when default-form-p `((:method ,type-or-lambda-list
                                               (declare (ignorable ,@type-or-lambda-list))
                                               ,default-form)))))
                (symbol
                 (let ((type type-or-lambda-list))
                   (unless (member type-or-lambda-list '(:reader :writer :accessor))
                     (error "For accessor specs, the second argument must be one of :reader, :writer, or :accessor. Got ~S instead." type))
                   `(progn
                      ,@(when (or (eq type :reader)
                                  (eq type :accessor))
                              `((defgeneric ,name (x)
                                  ,@(when documentation
                                          `((:documentation ,documentation)))
                                  ,@(when default-form-p `((:method (x)
                                                             (declare (ignore x))
                                                             ,default-form))))))
                      ,@(when (or (eq type :writer)
                                  (eq type :accessor))
                              `((defgeneric (setf ,name) (new-value x)
                                  ,@(when documentation
                                          `((:documentation ,documentation)))
                                  ,@(when default-form-p `((:method (new-value x)
                                                             (declare (ignore new-value x))
                                                             ,default-form))))))))))))
       t)))
