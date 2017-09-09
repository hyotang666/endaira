(defpackage :endaira.debugger.spec
  (:import-from :endaira.debugger
		#:*condition*
		#:*situations*
		)
  (:use :cl :jingoh :endaira.debugger :endaira.internals))
(in-package :endaira.debugger.spec)
(setup :endaira.debugger)

(requirements-about DEBUGGER-HOOK)

;;;; Description:
; endaira debugger.
; This works like around method.

#+syntax
(DEBUGGER-HOOK condition debugger-hook) ; => result

#?(let((*acc* nil))
    (debugger-hook (make-condition 'error) '#:dummy))
:invokes-debugger not
#?(let((*acc* (list(make-situation))))
    (debugger-hook (make-condition 'error) '#:dummy))
:invokes-debugger error

;;;; Arguments and Values:

; condition := condition otherwise error.
#?(let((*acc* (list(make-situation))))
    (debugger-hook '#:not-condition '#:dummy))
:signals error

; debugger-hook := T (ignored).

; result := NIL
#?(let((*acc* nil))
    (debugger-hook (make-condition 'error) '#:dummy))
=> NIL

;;;; Affected By:
; `ENDAIRA.INTERNALS:*ACC*`
; If `*ACC*` is nil, return nil normally.
; This means common lisp debugger will be invoked when `DEBUGGER-HOOK` is used to be bound to `*DEBUGGER-HOOK*`.
; If `*ACC*` is non nil, then makes restart named `INVOKE` and call `INVOKE-DEBUGGER`.
#?(let((*acc* (list(make-situation))))
    (debugger-hook (make-condition 'error) '#:dummy))
:invokes-debugger error
,:with-restarts endaira.debugger::invoke
,:test #`(equalp (list(make-situation)) *acc*)

;;;; Side-Effects:
; `ENDAIRA.INTERNALS:*CURRENT-PACKAGE*` is assigned with `*PACKAGE*` when *acc* is not NIL.
#?(let((*acc* (list(make-situation))))
    (debugger-hook (make-condition 'error) '#:dummy))
:invokes-debugger error
,:test #`(eq *package* *current-package*)

; `ENDAIRA.DEBUGGER::*CONDITION*` is bound by CONDITION when *acc* is not nil.
; Internal use only.

; `ENDAIRA.DEBUGGER::*SITUATIONS*` is assigned with value of `*ACC*`.
; This behavior is for debug use.
#? *situations* :satisfies #`(equalp $result (list (make-situation)))

; `ENDAIRA.INTERNALS:*ACC*` is assigned with nil.
; This behavior is cleaning up.
#? *acc* => NIL

; Program execution is discontinued, and the debugger is entered.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about INVOKE-ENDAIRA-DEBUGGER)

;;;; Description:
; Into endaira debugger's repl.

#+syntax
(INVOKE-ENDAIRA-DEBUGGER) ; => result

;;;; Arguments and Values:

; result := NIL

;;;; Affected By:

;;;; Side-Effects:
; In/out with `*debug-io*`.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEFOP :around
		    (let((endaira.debugger::*debug-ops*(make-hash-table :test #'equal)))
		      (call-body)))

;;;; Description:
; define debugger operation.

#+syntax
(DEFOP (name message) &body body) ; => result

;;;; Arguments and Values:

; name := keyword otherwise error.
#?(defop("not-keyword" "message")(index)index)
:signals error
; Not be evaluated.
#?(defop((intern "HOGE" :keyword)"message")(index)index)
:signals error

; message := string otherwise error.
#?(defop(:key :not-string)(index)index) :signals error
,:ignore-signals warning
; evaluated.
#?(defop(:key (format nil "message"))(index)index)
:be-the (cons string function)

; body := lambda-list implicit-progn
; LAMBDA-LIST shall accept one argument which is index of current frame.

; result := (cons keyword function)

;;;; Affected By:
; `endaira.debugger::*debug-ops*`, internal use.

;;;; Side-Effects:
; Modify `endaira.debugger::*debug-ops*`.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-BINDS)

;;;; Description:
; Print all frames binds.

#+syntax
(PRINT-BINDS binds) ; => result

#?(print-binds '((var 0)))
:outputs "VAR = 0"

;;;; Arguments and Values:

; binds := list (as alist) otherwise error.
#?(print-binds '#:not-list) :signals error
#?(print-binds '(#:not-alist)) :signals error
; bind := (var value) otherwise error.
#?(print-binds '((#:var . #:detted-is-invalid))) :signals error
; var := string-designator, otherwise error.
#?(print-binds '((0 '#:<---not-string-designator))) :signals error
; value := T

; result := NIL
#?(print-binds '((var 0))) => NIL
,:stream nil

;;;; Affected By:
; `*standard-output*`

;;;; Side-Effects:
; Outputs to `*standard-output*`.

;;;; Notes:
; BINDS is expected to be canonicalized already.

;;;; Exceptional-Situations:

(requirements-about PRINT-FUNCTION-TRACE)

;;;; Description:
; Print tracing function call.

#+syntax
(PRINT-FUNCTION-TRACE situation) ; => result

#?(print-function-trace (make-situation :name '#:name :binds '((#:bind #:value))))
:outputs "Into> (NAME #:VALUE)
"
;;;; Arguments and Values:

; situation := Situation object, otherwise error.
#?(print-function-trace '#:not-situation-object) :signals error

; result := NIL
#?(print-function-trace (make-situation)) => NIL
,:stream nil

;;;; Affected By:
; `*standard-output*`

;;;; Side-Effects:
; Outputs to `*standard-output*`.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-TRACING
		    :around
		    (let((*acc*(list(make-situation :name '#:name))))
		      (call-body)))

;;;; Description:
; Print tracing path from toplevel to error point.
#?(print-tracing *standard-output*)
:outputs "
NAME => ERROR"

#+syntax
(PRINT-TRACING &optional (*standard-output* *debug-io*)) ; => result

;;;; Arguments and Values:

; *standard-output* := output-stream otherwise error.
#?(with-input-from-string(s "dummy")
    (print-tracing s))
:signals error
#?(print-tracing '#:not-stream) :signals error
,:lazy t

; result := NIL
#?(print-tracing) => NIL
,:stream nil

;;;; Affected By:
; `*debug-io*` if `*standard-output*` is not specified.

;;;; Side-Effects:
; Outputs `*STANDARD-OUTPUT*`

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SKIP-PRINT)

;;;; Description:
; Condition for skipping needless printing.
;;;; Class Precedence List: (case in SBCL)
; skip-print condition slot-object t

;;;; Effective Slots:

;;;; Notes:

(requirements-about DEBUG-REPL :around
		    (let((*acc*(list (make-situation))))
		      (call-body)))

;;;; Description:
; REPL for endaira debugger.

#+syntax
(DEBUG-REPL &optional (index -1)) ; => result

;;;; Arguments and Values:

; index := integer otherwise error.
#?(debug-repl '#:not-integer) :signals error

; result := NIL, but into infinite looping. Does not return normally.

;;;; Affected By:
; `ENDAIRA.DEBUGGER::*ACC*` Internal use.

;;;; Side-Effects:
; In/out `*debug-io*`.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SITUATION<=INDEX
		    :around
		    (let((*acc*(list (make-situation :name 'first)
				     (make-situation :name 'second))))
		      (call-body)))

;;;; Description:
; Getting situation object from frame index.

#+syntax
(SITUATION<=INDEX index) ; => result

;;;; Arguments and Values:

; index := integer otherwise error
#?(situation<=index '#:not-integer) :signals error
,:ignore-signals warning

; result := Situation object
#?(situation<=index 0) :be-the situation

;;;; Affected By:
; `ENDAIRA.DEBUGGER::*ACC*` internal use.

;;;; Side-Effects:
; Refer `ENDAIRA.DEBUGGER::*ACC*`.

;;;; Notes:
; Minus integer is valid.
#?(situation<=index -1)
:satisfies #`(equalp $result (make-situation :name 'second))

;;;; Exceptional-Situations:
; If `ENDAIRA.DEBUGGER::*ACC*` is null, an error is signaled.
#?(let(*acc*)
    (situation<=index 1))
:signals error

(requirements-about MAKE-ENVIRONMENT
		    :around
		    (let((*acc* (list (make-situation :name 'first
						      :binds '((var1 1)
							       (var1.2 1.2)))
				      (make-situation :name 'second
						      :binds '((var2 2))))))
		      (call-body)))


;;;; Description:
; Accept frame index, then return two values.
; 1. Environment alist.
; 2. Situation names.

#?(make-environment 0)
:values (((var1 1)(var1.2 1.2))
	 (FIRST FIRST))

#+syntax
(MAKE-ENVIRONMENT index) ; => result

;;;; Arguments and Values:

; index := integer otherwise error.
#?(make-environment '#:not-integer) :signals error
; Minus index is valid.
#?(make-environment -1)
:values ( ((var2 2)(var1 1)(var1.2 1.2))
	  (SECOND FIRST FIRST)
	  )

; result := (values environment* name+)
; environment := (var value)
; var := string-designator
; value := T
; name := string-designator

;;;; Affected By:
; `ENDAIRA.DEBUGGER::*ACC*`

;;;; Side-Effects:
; Refer `ENDAIRA.DEBUGGER::*ACC*`.

;;;; Notes:
; Sub-situations are also targets.
; It means lower (specific) situation includes higher (to toplevel) situations.
; It is similar with subclass includes superclass.

;;;; Exceptional-Situations:
; If `ENDAIRA.DEBUGGER::*ACC* is null, an error is signaled.
#?(let(*acc*)
    (make-environment 0))
:signals error

(requirements-about SET-PREFIX)

;;;; Description:
; Return environment alist.

#+syntax
(SET-PREFIX binds names) ; => result

#?(set-prefix '((var 0)) '(first))
=> (("FIRST/VAR" 0))
,:test equal
;;;; Arguments and Values:

; binds := ((var value)*)
; var := string-designator
; value := T

; names := (string-designator*)

; result := ((var value)*)

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; If BINDS and/or NAMES are null, nil is returned.
#?(set-prefix () '(name)) => NIL
#?(set-prefix '((var 0)) ()) => NIL
#?(set-prefix () ()) => NIL

;;;; Exceptional-Situations:

