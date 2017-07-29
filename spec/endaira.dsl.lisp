(defpackage :endaira.dsl.spec
  (:use :cl :jingoh :endaira.dsl))
(in-package :endaira.dsl.spec)
(setup :endaira.dsl)

(requirements-about WITH-HANDLER-NAMED)

;;;; Description:
; Set named handler binding.

#+syntax
(WITH-HANDLER-NAMED name &body body) ; => result

#?(with-handler-named 0 1)
:expanded-to
(let((var(loop :for arg :in 'nil
	       :for val :in (list)
	       :collect (list arg (if *copy*
				    `(quote ,(copy val))
				    val)))))
  (handler-bind((error(lambda(condition)
			(declare(ignore condition))
			(push (make-situation :name '0
					      :form '1
					      :binds var)
			      *acc*))))
    1))

;;;; Arguments and Values:

; name := [ identifier | parameters ]
; identifier := atom
; parameters := (identifier var*)
; var := symbol

; body := implicit progn

; result := return value of BODY.

;;;; Affected By:
;;; Compile time
; none
;;; Run time
; `*COPY*`

;;;; Side-Effects:
;;; Run time
; Storing `SITUATION` object to `*ACC*` when error is signaled.

;;;; Notes:
; When `*COPY*` is non-NIL value, `VAL` is deeply copied.
; This means, when `VAL` is circle structure, `COPY` never return and becomes stack overflow.

;;;; Exceptional-Situations:

(requirements-about +BINDS+)

;;;; Description:
; Internal use for variable injection.
; See #P"builder/endaira-builder.lisp" especially `BREAK` as example.

;;;; Value type is SYMBOL
#? +BINDS+ :be-the symbol

; Initial value is #:BINDS

;;;; Affected By:

;;;; Notes:
