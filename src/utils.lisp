(cl:defpackage #:conserv.utils
  (:use #:cl #:alexandria)
  (:export #:make-queue
           #:enqueue
           #:dequeue
           #:queue-empty-p
           #:dequeue-all
           #:defprotocol
           #:*default-external-format*))
(cl:in-package #:conserv.utils)

(defvar *default-external-format* :utf-8)

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

(defun dequeue-all (q)
  (prog1 (car q)
    (setf (car q) nil
          (cdr q) nil)))

(defmacro defprotocol (name typevars genfun-specs &rest protocol-options)
  "syntax: (defprotocol <name> (<typevar>*)
             ((<genfun-name> (<var>*|(<var> <typevar>)*)
               [:accessor <boolean>]
               [:default-form <default-form>]
               [:documentation <docstring>])*)
             (<option-name> . <option-args>*)*

   where

   <name> - Symbol name of the protocol, unevaluated.
   <typevar> - symbol denoting a type variable. Used for specifying desired types for generic functions.
   <genfun-name> - A valid function name to be given to the generic function.
   <var> - Variable to be used for the generic function lambda list.
   <default-form> - Any valid lisp form.
   <docstring> - A string.
   <option-name> - One of :prefix or :documentation
   <option-args> - Expected arguments for the given option.

DEFPROTOCOL defines a protocol (not currently a first-class object), which is, logically speaking, a
collection of generic functions that define an expected interface that must be implemented through
DEFMETHOD in order for behavior on a set of objects to be considered complete. On top of grouping
generic function definitions logically, the DEFPROTOCOL facility provides some extra
conveniences.

If :accessor is provided in a genfun spec, a second generic function with lambda-list (new-value
. original-lambda-list) will be generated. To provide a default form for this automagic genfun,
use :accessor-default-form, which works like :default-form. :default-form will still provide its
form to the standard 'reader' function.

The :default-form argument, if provided, defines a default method to the generic function with the
provided form as its body. \"Default\" in this case means that all primary arguments will be
specialized on T. For genfun specs with lambda-lists, the variables defined in the lambda list are
made available to the form. example: (frob (thing) :default-form (error \"Function not implemented
for object of class ~S\" (class-of thing)))

If the :prefix option is provided, the single symbol argument to :prefix will be concatenated to all
the generic function names in the protocol.

Finally, the :documentation argument for both the DEFPROTOCOL form and genfun specs are simply meant to
provide useful documentation to the next victim of your code. In the case of genfun-specs,
the :documentation argument is used as the :documentation argument to the defgeneric form.
"
  (declare (ignore name))
  (loop for (option-name . nil) in protocol-options
     unless (member option-name '(:documentation :prefix))
     do (error "Unrecognized defprotocol option: ~S" option-name))
  `(progn
     ,@(loop for spec in genfun-specs
          collect
          (destructuring-bind (name proto-lambda-list &key
                                    documentation
                                    (default-form nil default-form-p)
                                    accessorp
                                    (accessor-default-form nil accessor-default-form-p))
              spec
            (unless (or (symbolp name)
                        (and (listp name)
                             (eq 'setf (car name))
                             (symbolp (cadr name))))
              (error "Invalid function name: ~S" name))
            (when-let (prefix (cadr (assoc :prefix protocol-options)))
              (setf name
                    (if (listp name)
                        (list 'setf (intern (concatenate 'string (string prefix) (string (cadr name)))))
                        (intern (concatenate 'string (string prefix) (string name))))))
            (let ((lambda-list (loop for spec in proto-lambda-list
                                    collect (etypecase spec
                                              (symbol spec)
                                              (list (assert (member (cadr spec) typevars) () "~S is not listed as part of the protocol's type variables." (cadr spec))
                                                    (car spec))))))
              `(progn
                 (defgeneric ,name ,lambda-list
                   ,@(when documentation `((:documentation ,documentation)))
                   ,@(when default-form-p `((:method ,lambda-list
                                              ;; TODO - (declare (ignorable foo &key bar))? :)
                                              (declare (ignorable ,@lambda-list))
                                              ,default-form))))
                 ,@(when accessorp
                     (let ((lambda-list (cons 'new-value lambda-list)))
                       `((defgeneric (setf ,name) ,lambda-list
                           ,@(when documentation `((:documentation ,documentation)))
                           ,@(when accessor-default-form-p `((:method ,lambda-list
                                                               ;; TODO - (declare (ignorable foo &key bar))? :)
                                                               (declare (ignorable ,@lambda-list))
                                                               ,accessor-default-form)))))))))))
     t))
