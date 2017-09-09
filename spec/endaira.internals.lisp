(defpackage :endaira.internals.spec
  (:use :cl :jingoh :endaira.internals))
(in-package :endaira.internals.spec)
(setup :endaira.internals)

(requirements-about *ACC*)

;;;; Description:
; Accumulate `SITUATION`s.

;;;; Value type is LIST
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
; ECL debugger binds `*package*` with cl-user in debugger.

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

; name := (OR SYMBOL STRING (CONS (EQL SETF) T)), otherwise unspecified.
#?(make-situation :name 0) => unspecified

; binds := list, otherwise unspecified.
#?(make-situation :binds :not-list) => unspecified

; form := T

; result := situation

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(common-requirements-about (SITUATION-NAME NAME<=SITUATION)
			   :as op)

;;;; Description:
; Get its name from situation.

#+syntax
(OP sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := situation, otherwise error.
#?(op :not-situation-object) :signals error
,:lazy T

; result := (or symbol string (cons (eql setf) t))

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(common-requirements-about (SITUATION-BINDS BINDS<=SITUATION)
			   :as op)

;;;; Description:
; Get binds from situation.

#+syntax
(op sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := situation, otherwise error
#?(op :not-situation-object) :signals error
,:lazy T

; result := ((var value)*)
; var := string-designator
; value := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(common-requirements-about (SITUATION-FORM FORM<=SITUATION)
			   :as op)

;;;; Description:
; Get form from situation.

#+syntax
(OP sb-kernel:instance) ; => result

;;;; Arguments and Values:

; instance := situation, otherwise error.
#?(op :not-situation-object) :signals error
,:lazy T

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

