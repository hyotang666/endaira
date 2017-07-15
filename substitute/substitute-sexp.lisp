(defpackage :endaira.substitute-sexp(:use :cl)
  (:import-from :lambda-list #:Vars<=lambda-list)
  (:export
    #:substitute-sexp
    ))
(in-package :endaira.substitute-sexp)

(defun substitute-sexp(form binds &optional env)
  (labels((EXPAND(env form &optional(expandedp t))
	    (if(not expandedp)
	      form
	      (if(ENDAIRA-FORM-P form) ; we should hide WITH-HANDLER-NAMED.
		(EXPAND env(LISPIZE form))
		(multiple-value-call #'EXPAND env (macroexpand-1 form env)))))
	  (ENDAIRA-FORM-P(form)
	    (and (typep form '(cons symbol t))
		 (eq #.(find-package :endaira-core)
		     (symbol-package(car form)))))
	  (LISPIZE(form)
	    `(,(find-symbol(symbol-name(car form)):cl),@(cdr form)))
	  (LOCAL-VALUE(symbol binds)
	    (let((bind(assoc symbol binds)))
	      (if bind ; to avoid semi-boolean issue.
		(cadr bind)
		symbol)))
	  )
    (let((expanded(EXPAND env form)))
      (etypecase expanded
	((and symbol (not(or boolean keyword))) ; it is var.
	 (LOCAL-VALUE expanded binds))
	(atom expanded)
	(list (let*((elt(car expanded))
		    (substituter(substituter elt)))
		(if substituter
		  (funcall substituter expanded binds env)
		  `(,elt ; to avoiding expand op conflicts with symbol-macro.
		     ,@(substitute-each(cdr expanded)binds env)))))))))

(defun substitute-each(forms binds env)
  (loop :for form :in forms
	:collect(substitute-sexp form binds env)))

(defvar *substituters*(make-hash-table :test #'eq))

(defun substituter(key)
  (when(symbolp key)
    (gethash key *substituters*)))

(eval-when(:compile-toplevel :load-toplevel :execute)
  (defmacro defsubst(name &body body)
    `(SETF(GETHASH ',name *SUBSTITUTERS*)
       (LAMBDA ,@body))))

(defun copy-subst(dest src)
  (setf(gethash dest *substituters*)(substituter src)))

(defsubst let(whole binds env)
  (destructuring-bind(op bindings . body)whole
    `(,op ,(mapcar(lambda(bind)
		    (if(symbolp bind)
		      bind
		    `(,(car bind),(substitute-sexp (cadr bind)binds env))))
	     bindings)
	  ,@(substitute-each body binds env))))

(copy-subst 'let* 'let)

(defsubst quote(whole binds env)
  (declare(ignore binds env))
  whole)

(progn . #.(mapcar(lambda(dest)
		    `(copy-subst ',dest 'quote))
	     '(go load-time-value)))

(defsubst function(whole binds env)
  (destructuring-bind(op name . imple)whole
    (if(and (listp name)(eq 'lambda(car name)))
      `(,op ,(funcall(substituter 'lambda)name binds env) ,@imple)
      whole)))

(defsubst lambda(whole binds env)
  (labels((SHADOWING(lambda-list env)
	    (labels((REC(list acc)
		      (if(endp list)
			acc
			(REC(cdr list)(remove(car list)acc
					:key #'car :test #'eq)))))
	      (REC(Vars<=lambda-list lambda-list)env)))
	  )
    (destructuring-bind(op lambda-list . body)whole
      `(,op ,lambda-list ,@(substitute-each body
					    (SHADOWING lambda-list binds)
					    env)))))

(defsubst setq(whole binds env)
  (destructuring-bind(op . bindings)whole
    `(,op ,@(loop :for (var . rest) :on bindings :by #'cddr
		  :nconc`(,var ,(substitute-sexp (car rest)binds env))))))

(copy-subst 'psetq 'setq)

(defsubst return-from(whole binds env)
  (destructuring-bind(op first second)whole
    `(,op ,first ,(substitute-sexp second binds env))))

(progn . #.(mapcar(lambda(dest)
		    `(copy-subst ',dest 'return-from))
	     '(throw the multiple-value-setq)))

(defsubst flet(whole binds env)
  (destructuring-bind(op first . rest)whole
    `(,op ,first ,@(substitute-each rest binds env))))

(progn . #.(mapcar(lambda(dest)
		    `(copy-subst ',dest 'flet))
	     '(labels eval-when block catch multiple-value-bind)))

(eval-when(:load-toplevel :compile-toplevel :execute)
  (defmacro get-env(&environment env)
    `',env))

(defsubst macrolet(whole binds env)
  (destructuring-bind(op bindings . body)whole
    (eval`(,op ,bindings (substitute-sexp ; to expand lexical local macro.
			   (substitute-sexp ; to expand local macro.
			     ',(if(cdr body) ; BODY has some forms,
				 ; and first form may DECLARE, so...
				 (cons 'locally body)
				 (car body))
			     ',binds
			     (get-env))
			   ',binds
			   ',env)))))

(copy-subst 'symbol-macrolet 'macrolet)

(defsubst tagbody(whole binds env)
  (destructuring-bind(op . rest)whole
    `(,op ,@(mapcar(lambda(x)
		     (if(typep x '(OR SYMBOL CHARACTER INTEGER))
		       x
		       (substitute-sexp x binds env)))
	      rest))))
