(in-package :endaira.debugger)

(defvar *situations* nil) ; for debug use.

(defun debugger-hook(condition debugger-hook)
  (declare(ignore debugger-hook))
  (unwind-protect
    (when *Acc*
      (setf *Current-package* *package*) ; for ecl
      (print-tracing)
      (loop
	(catch'Quit-signal ; for ccl
	  (handler-case(restart-case(invoke-debugger condition)
			 (invoke()
			   :report "Invoke endaira debugger"
			   (let((*condition* condition))
			     (invoke-endaira-debugger))))
	    (Quit-signal()(return-message))))))
    (shiftf *situations* *Acc* nil)))

(defun invoke-endaira-debugger()
  (format *debug-io* "~&;; Type :Q to quit, Type :H to help.~%")
  (debug-repl))

(if *debugger-hook* ; somebody already set it, but...
  (if(eq 'debugger-hook (Function-name *debugger-hook*)) ; it's me!
    #0=(setq *debugger-hook* #'debugger-hook) ; reset is not problem.
    (when(y-or-n-p "ENDAIRA: Somebody already set *DEBUGGER-HOOK*. : ~S~%~9TReally you want to set ENDAIRA hook?"*debugger-hook*)
      #0#))
  ; else nobody set *DEBUGGER-HOOK*, so...
  #0#) ; set it!
