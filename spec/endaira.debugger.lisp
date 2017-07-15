(defpackage :endaira.debugger.spec
  (:use :cl :jingoh :endaira.debugger))
(in-package :endaira.debugger.spec)
(setup :endaira.debugger)

(requirements-about DEBUGGER-HOOK)

;;;; Description:
; endaira debugger.

#+syntax
(DEBUGGER-HOOK condition debugger-hook) ; => result

;;;; Arguments and Values:

; condition := 

; debugger-hook := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INVOKE-ENDAIRA-DEBUGGER)

;;;; Description:
; Into endaira debugger's repl.

#+syntax
(INVOKE-ENDAIRA-DEBUGGER) ; => result

;;;; Arguments and Values:

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFOP)

;;;; Description:
; define debugger operation.

#+syntax
(DEFOP (name message) &body body) ; => result

;;;; Arguments and Values:

; name := 

; message := 

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-BINDS)

;;;; Description:

#+syntax
(PRINT-BINDS binds) ; => result

;;;; Arguments and Values:

; binds := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-FUNCTION-TRACE)

;;;; Description:

#+syntax
(PRINT-FUNCTION-TRACE situation) ; => result

;;;; Arguments and Values:

; situation := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-TRACING)

;;;; Description:

#+syntax
(PRINT-TRACING &optional (*standard-output* *debug-io*)) ; => result

;;;; Arguments and Values:

; *standard-output* := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SKIP-PRINT)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; skip-print condition slot-object t

;;;; Effective Slots:

;;;; Notes:

(requirements-about DEBUG-REPL)

;;;; Description:

#+syntax
(DEBUG-REPL &optional (index -1)) ; => result

;;;; Arguments and Values:

; index := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SITUATION<=INDEX)

;;;; Description:

#+syntax
(SITUATION<=INDEX index) ; => result

;;;; Arguments and Values:

; index := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about MAKE-ENVIRONMENT)

;;;; Description:

#+syntax
(MAKE-ENVIRONMENT index) ; => result

;;;; Arguments and Values:

; index := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SET-PREFIX)

;;;; Description:

#+syntax
(SET-PREFIX binds names) ; => result

;;;; Arguments and Values:

; binds := 

; names := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

