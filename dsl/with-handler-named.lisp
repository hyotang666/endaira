(defpackage :endaira.dsl(:use :cl :endaira.internals)
  (:import-from :endaira.copy #:copy)
  (:export
    #:with-handler-named
    #:+binds+
    #:*copy*
    ))
(in-package :endaira.dsl)

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defvar +binds+(make-symbol "BINDS")))
(defparameter *copy* T)

(setf (symbol-value +binds+) nil)

(defmacro with-handler-named(name &body body)
  "Set named handler binding.
  syntax (WITH-HANDLER-NAMED name &BODY body)
  name = (OR identifier params)
  identifier = ATOM
  params = (CONS IDENTIFIER vars)
  vars = SYMBOL*
  body = S-EXPRESSION*"
  (multiple-value-bind(identifier args)(parse-name name)
    (let((condition(gensym"CONDITION"))
	 (form(if(cdr body) ; have some form
		(cons 'progn body)
		(car body))))
      `(LET((,+binds+ (LOOP :FOR ARG :IN ',args
			    :FOR VAL :IN (LIST ,@args)
			    :COLLECT (LIST ARG (IF *COPY*
						   `(QUOTE ,(COPY VAL))
						   VAL)))))
	 (HANDLER-BIND((ERROR(LAMBDA(,condition)
			       ; don't touch condition!,
			       ; we need to keep condition's identity.
			       (DECLARE(IGNORE ,condition))
			       (PUSH(MAKE-SITUATION
				      :NAME ',identifier
				      :FORM ',form
				      :BINDS ,+binds+)
				 *ACC*))))
	   ,@body)))))

(defun parse-name(name)
  (if(listp name)
    (values(car name)(cdr name))
    (values name)))
