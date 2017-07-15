(defpackage :endaira.dsl.spec
  (:use :cl :jingoh :endaira.dsl))
(in-package :endaira.dsl.spec)
(setup :endaira.dsl)

(requirements-about WITH-HANDLER-NAMED)

;;;; Description:
; Set named handler binding.
;   syntax (WITH-HANDLER-NAMED name &BODY body)
;   name = (OR identifier params)
;   identifier = ATOM
;   params = (CONS IDENTIFIER vars)
;   vars = SYMBOL*
;   body = S-EXPRESSION*

#+syntax
(WITH-HANDLER-NAMED name &body body) ; => result

;;;; Arguments and Values:

; name := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about +BINDS+)

;;;; Description:

;;;; Value type is SYMBOL
;#? +BINDS+ :be-the ???

; Initial value is #:BINDS

;;;; Affected By:

;;;; Notes:

