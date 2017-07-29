(defpackage :endaira.internals.spec
  (:use :cl :jingoh :endaira.internals))
(in-package :endaira.internals.spec)
(setup :endaira.internals)

(requirements-about *ACC*)

;;;; Description:
; Acculate `SITUATION`s.

;;;; Value type is NULL
#? *ACC* :be-the LIST

; Initial value is NIL

;;;; Affected By:
; `endaira.dsl:with-handler-named` works as setter.

; `endaira.debugger:debugger-hook` works as cleaner.

;;;; Notes:

(requirements-about *CURRENT-PACKAGE*)

;;;; Description:
; Storing current package.

;;;; Value type is PACKAGE
#? *CURRENT-PACKAGE* :be-the package

; Initial value is #<PACKAGE "ENDAIRA.INTERNALS">

;;;; Affected By:
; `endaira.debugger:debugger-hook`

;;;; Notes:
; For ECL.
; ECL deubgger binds `*package*` with cl-user in debugger.

(requirements-about QUIT-SIGNAL)

;;;; Description:
; Internal use for quit endaira debug repl.

;;;; Class Precedence List: (case in SBCL)
; quit-signal condition slot-object t

;;;; Effective Slots:

;;;; Notes:

(requirements-about SITUATION)

;;;; Description:
; Consider this is something like stack frame.

;;;; Class Precedence List: (case in SBCL)
; situation structure-object slot-object t

;;;; Effective Slots:

; NAME [Type] (OR SYMBOL STRING (CONS (EQL SETF) T))
; Situation name.

; BINDS [Type] LIST
; Variable bindings if any.

; FORM [Type] T
; S-Expression as stack frame.

;;;; Notes:

(requirements-about MAKE-SITUATION)

;;;; Description:
; Constructor

#+syntax
(MAKE-SITUATION &key
		((:name #:name) nil)
		((:binds #:binds) nil)
		((:form #:form) nil))
; => result
#?(make-situation) :be-the situation

;;;; Arguments and Values:

; name := 

; binds := 

; form := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SITUATION-NAME)

;;;; Description:

#+syntax
(SITUATION-NAME sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about NAME<=SITUATION)

;;;; Description:

#+syntax
(NAME<=SITUATION #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SITUATION-BINDS)

;;;; Description:

#+syntax
(SITUATION-BINDS sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about BINDS<=SITUATION)

;;;; Description:

#+syntax
(BINDS<=SITUATION #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SITUATION-FORM)

;;;; Description:

#+syntax
(SITUATION-FORM sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about FORM<=SITUATION)

;;;; Description:

#+syntax
(FORM<=SITUATION #:arg) ; => result

;;;; Arguments and Values:

; arg := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

