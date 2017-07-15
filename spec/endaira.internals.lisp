(defpackage :endaira.internals.spec
  (:use :cl :jingoh :endaira.internals))
(in-package :endaira.internals.spec)
(setup :endaira.internals)

(requirements-about *ACC*)

;;;; Description:

;;;; Value type is NULL
;#? *ACC* :be-the ???

; Initial value is NIL

;;;; Affected By:

;;;; Notes:

(requirements-about *CURRENT-PACKAGE*)

;;;; Description:

;;;; Value type is PACKAGE
;#? *CURRENT-PACKAGE* :be-the ???

; Initial value is #<PACKAGE "ENDAIRA.INTERNALS">

;;;; Affected By:

;;;; Notes:

(requirements-about QUIT-SIGNAL)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; quit-signal condition slot-object t

;;;; Effective Slots:

;;;; Notes:

(requirements-about SITUATION)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; situation structure-object slot-object t

;;;; Effective Slots:

; NAME [Type] (OR SYMBOL STRING (CONS (EQL SETF) T))

; BINDS [Type] LIST

; FORM [Type] T

;;;; Notes:

(requirements-about MAKE-SITUATION)

;;;; Description:

#+syntax
(MAKE-SITUATION &key ((:name #:name) nil) ((:binds #:binds) nil) ((:form
                                                                   #:form)
                                                                  nil)) ; => result

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

