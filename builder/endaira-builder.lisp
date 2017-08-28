(defpackage :endaira.builder(:use :cl :endaira.dsl)
  (:import-from :endaira.internals #:*Acc*)
  (:import-from :endaira.debugger #:Print-binds)
  (:import-from :lambda-list #:Vars<=lambda-list)
  )

(in-package :endaira.builder)

(defmacro endaira.core::defun(name lambda-list &body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    `(DEFUN ,name ,lambda-list
       ,@documentation
       ,@declaration
       (WITH-HANDLER-NAMED,(cons name
				 (remove-ignored-vars (Vars<=lambda-list lambda-list)
						      declaration))
	 ,@body))))

(defun parse-body(src)
  (loop :for top :in src
	:if(stringp top) :collect top :into documentation
	:else :if(and (consp top)
		    (string= '#:declare(car top)))
	:collect top :into declaration
	:else :collect top :into body
	:finally (return(values documentation declaration body))))

(defun remove-ignored-vars(vars declares)
  (labels((IGNORED-VARS(declares)
	    (loop :for (nil . body) :in declares
		  :nconc(loop :for (clause . args) :in body
			      :when(eq 'ignore clause)
			      :append args)))

	  )
    (set-difference vars(IGNORED-VARS declares))))

(defmacro endaira.core::defmacro(name lambda-list &body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    `(DEFMACRO ,name ,lambda-list
       ,@documentation
       ,@declaration
       (WITH-HANDLER-NAMED,(cons (format nil "~A[expand]" name)
				 (remove-ignored-vars (Vars<=lambda-list lambda-list :as :macro)
						      declaration))
	 ,@body))))

(defmacro endaira.core::defmethod(name &body body)
  (flet((ENSURE-SPECIALIZER(body)
	  (typecase(car body)
	    (symbol (rplaca body(list(car body))))
	    (list(push nil body))
	    (t(error(format nil "~&DEFMETHOD signaled!~&~
			    This is invalid form : ~A"body))))))
    (destructuring-bind(specializer lambda-list . body)(ENSURE-SPECIALIZER body)
      (multiple-value-bind(documentation declaration body)(parse-body body)
	`(DEFMETHOD ,name ,@specializer ,lambda-list
	   ,@documentation
	   ,@declaration
	   (WITH-HANDLER-NAMED,(cons name
				     (remove-ignored-vars (Vars<=lambda-list lambda-list :as :method)
							  declaration))
	     ,@body))))))

(progn . #.(mapcar
	     (lambda(op)
	       `(DEFMACRO,(intern(symbol-name op):endaira.core)(FNS &BODY BODY)
		  `(,',op,(LOOP :FOR (NAME LAMBDA-LIST . BODY):IN FNS
				:COLLECT
				(MULTIPLE-VALUE-BIND(DOCUMENTATION DECLARATION BODY)(PARSE-BODY BODY)
				  `(,NAME ,LAMBDA-LIST
					  ,@DOCUMENTATION
					  ,@DECLARATION
					  (WITH-HANDLER-NAMED,(CONS (FORMAT NIL "~S[~A]"NAME ',op)
								    (REMOVE-IGNORED-VARS(VARS<=LAMBDA-LIST LAMBDA-LIST)DECLARATION))
					    ,@BODY))))
		     ,@BODY)))
	     '(flet labels macrolet)))

#++
(defmacro endaira.core::flet(fns &body body)
  `(FLET,(loop :for (name lambda-list . body) :in fns :collect
	       (multiple-value-bind(documentation declaration body)(parse-body body)
		 `(,name,lambda-list
		    ,@documentation
		    ,@declaration
		    (WITH-HANDLER-NAMED,(cons (format nil "~A[FLET]"name)
					      (remove-ignored-vars (Vars<=lambda-list lambda-list)
								   declaration))
		      ,@body))))
     ,@body))

(defmacro endaira.core::let(binds &body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    (declare(ignore documentation))
    `(LET,(mapcar (lambda(bind)
		    (if(symbolp bind)
		      bind
		      (let((name(car bind)))
			`(,name(WITH-HANDLER-NAMED,(format nil "LET[bind for ~A]"name)
				 ,@(cdr bind))))))
		  binds)
       ,@declaration
       (WITH-HANDLER-NAMED,(cons (format nil "LET[body]")
				 (remove-ignored-vars (mapcar #'ensure-car binds)
						      declaration))
	 ,@body))))

(defun ensure-car(arg)
  (if(atom arg)
    arg
    (car arg)))

(defmacro endaira.core::let*(binds &body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    (declare(ignore documentation))
    `(LET*,(mapcar (lambda(bind)
		     (etypecase bind
		       (symbol bind)
		       (list
			 (let((var(car bind)))
			   `(,var(WITH-HANDLER-NAMED,(cons (format nil "LET*[bind for ~A]"var)
							   (seen-var var binds))
				   ,@(cdr bind)))))))
		   binds)
       ,@declaration
       (WITH-HANDLER-NAMED,(cons (format nil "LET*[body]")
				 (remove-ignored-vars (mapcar #'ensure-car binds)
						      declaration))
	 ,@body))))

(defun seen-var(var binds)
  (loop :for #0=#:temp :in binds
	:for elt = (ensure-car #0#)
	:if(eq elt var)
	:do (return result)
	:else :collect elt :into result
	:finally (error "Invalid form for binds. : ~S"binds)))

(defmacro endaira.core::multiple-value-bind(binds init-form &body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    (declare(ignore documentation))
    `(MULTIPLE-VALUE-BIND ,binds(WITH-HANDLER-NAMED"MULTIPLE-VALUE-BIND(init)"
				  ,init-form)
       ,@declaration
       (WITH-HANDLER-NAMED,(cons "MULTIPLE-VALUE-BIND[body]"
				 (remove-ignored-vars binds declaration))
	 ,@body))))

(defmacro endaira.core::destructuring-bind(binds init-form &body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    (declare(ignore documentation))
    `(DESTRUCTURING-BIND,binds(WITH-HANDLER-NAMED"DESTRUCTURING-BIND[init]"
				,init-form)
       ,@declaration
       (WITH-HANDLER-NAMED,(cons "DESTRUCTURING-BIND[body]"
				 (remove-ignored-vars (Vars<=lambda-list binds :as :macro)
						      declaration))
	 ,@body))))

(defmacro endaira.core::ignore-errors(&body body)
  `(LET(*ACC*)
     (IGNORE-ERRORS ,@body)))

(defmacro endaira.core::handler-case(form &body body)
  `(LET(*ACC*)
     (HANDLER-CASE ,form ,@body)))

(defmacro endaira.core::break(format-control &rest format-args)
  `(WITH-SIMPLE-RESTART(CONTINUE "Return from break")
     (FORMAT *ERROR-OUTPUT* "~%BREAK was called.~@[~&Current binds are~&~]"
	     ,+binds+)
     (PRINT-BINDS ,+binds+)
     (INVOKE-DEBUGGER(MAKE-CONDITION 'SIMPLE-ERROR
				     :FORMAT-CONTROL ,format-control
				     :FORMAT-ARGUMENTS (LIST ,@format-args)))))

(defmacro endaira.core::dolist((var init-form &optional return)&body body)
  (multiple-value-bind(documentation declaration body)(parse-body body)
    (declare(ignore documentation))
    `(DOLIST(,var (WITH-HANDLER-NAMED"DOLIST[init]"
		    ,init-form)
		  ,return)
       ,@declaration
       (WITH-HANDLER-NAMED,(cons "DOLIST[body]"
				 (remove-ignored-vars (list var)declaration))
	 ,@body))))

(defmacro endaira.core::cond(&rest clauses)
  `(COND
     ,@(loop :for (pred . body) :in clauses
	     :collect `(,pred (WITH-HANDLER-NAMED,(format nil "COND:~S"pred)
				,@body)))))

(progn . #.(mapcar
	     (lambda(op)
	       `(DEFMACRO,(intern(string op):endaira.core)(VAR &REST CLAUSES)
		  `(,',op ,VAR
			  ,@(LOOP :FOR (P . BODY) :IN CLAUSES
				  :COLLECT `(,P (WITH-HANDLER-NAMED,(FORMAT NIL"~A:~S"',op ,'P)
						    ,@BODY))))))
	     '(typecase etypecase ctypecase case ecase ccase)))
