(common-lisp:defpackage :endaira.core.spec
  (:use :cl :jingoh :endaira.core)
  )
(in-package :endaira.core.spec)
(setup :endaira.core)

(requirements-about DEFUN)

;;;; Description:

#+syntax
(DEFUN endaira.builder::name endaira.builder::lambda-list common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; name := 

; lambda-list := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFMACRO)

;;;; Description:

#+syntax
(DEFMACRO endaira.builder::name endaira.builder::lambda-list common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; name := 

; lambda-list := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFMETHOD)

;;;; Description:

#+syntax
(DEFMETHOD endaira.builder::name common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; name := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FLET)

;;;; Description:

#+syntax
(FLET endaira.builder::fns common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; fns := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LABELS)

;;;; Description:

#+syntax
(LABELS endaira.builder::fns common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; fns := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MACROLET)

;;;; Description:

#+syntax
(MACROLET endaira.builder::fns common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; fns := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LET)

;;;; Description:

#+syntax
(LET endaira.builder::binds common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; binds := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about LET*)

;;;; Description:

#+syntax
(LET* endaira.builder::binds common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; binds := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MULTIPLE-VALUE-BIND)

;;;; Description:

#+syntax
(MULTIPLE-VALUE-BIND endaira.builder::binds endaira.builder::init-form common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; binds := 

; init-form := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DESTRUCTURING-BIND)

;;;; Description:

#+syntax
(DESTRUCTURING-BIND endaira.builder::binds endaira.builder::init-form common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; binds := 

; init-form := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DOLIST)

;;;; Description:

#+syntax
(DOLIST (endaira.builder::var endaira.builder::init-form common-lisp:&optional
         common-lisp:return) common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; var := 

; init-form := 

; return := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about COND)

;;;; Description:

#+syntax
(COND common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CASE)

;;;; Description:

#+syntax
(CASE endaira.builder::var common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; var := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ECASE)

;;;; Description:

#+syntax
(ECASE endaira.builder::var common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; var := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CCASE)

;;;; Description:

#+syntax
(CCASE endaira.builder::var common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; var := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about TYPECASE)

;;;; Description:

#+syntax
(TYPECASE endaira.builder::var common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; var := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about ETYPECASE)

;;;; Description:

#+syntax
(ETYPECASE endaira.builder::var common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; var := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CTYPECASE)

;;;; Description:

#+syntax
(CTYPECASE endaira.builder::var common-lisp:&rest endaira.builder::clauses) ; => result

;;;; Arguments and Values:

; var := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about IGNORE-ERRORS)

;;;; Description:

#+syntax
(IGNORE-ERRORS common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about HANDLER-CASE)

;;;; Description:

#+syntax
(HANDLER-CASE endaira.builder::form common-lisp:&body endaira.builder::body) ; => result

;;;; Arguments and Values:

; form := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about BREAK)

;;;; Description:

#+syntax
(BREAK endaira.builder::format-control common-lisp:&rest endaira.builder::format-args) ; => result

;;;; Arguments and Values:

; format-control := 

; format-args := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

