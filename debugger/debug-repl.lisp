(in-package :endaira.debugger)

(define-condition skip-print()())

(defvar *debug-ops*(make-hash-table :test #'eq))

(defun debug-repl(&optional(index -1))
  (labels((PROMPT(index)
	    (format *debug-io* "~&ENDAIRA-REPL[~S]~&> "(NTH-FRAME index))
	    (force-output)
	    (read *debug-io*))
	  (NTH-FRAME(index)
	    (mod index (length *Acc*)))
	  (DEBUG-EVAL(sexp index)
	    (setf(symbol-value '-)sexp)
	    (let((results(multiple-value-list(funcall (DEBUG-OPERATOR sexp)
						      index))))
	      (shiftf /// // / results)
	      (shiftf *** ** * (car results))
	      (shiftf +++ ++ + sexp)
	      (values-list results)))
	  (DEBUG-OPERATOR(sexp)
	    (cdr(gethash sexp *debug-ops*
			 (cons '#:dummy(lambda(index)
					 (eval(WRAP-WITH-FRAME sexp index)))))))
	  (WRAP-WITH-FRAME(sexp index) ; making nested LET form.
	    (labels((REC(situations)
		      (if(endp situations)
			`(,sexp)
			(let((situation(car situations)))
			  `((let,(Binds<=situation situation)
			      ,(DECLARE-IGNORABLES(Binds<=situation situation))
			      ,@(REC(cdr situations)))))))
		    (DECLARE-IGNORABLES(binds)
		      `(declare(ignorable ,@(mapcar #'car binds))))
		    )
	      (car(REC(subsituations index)))))
	  )
    (let((*standard-output* *debug-io*) ; as canonicalize.
	 (*print-pretty* t) ; for sbcl.
	 (*print-length* nil) ; for sbcl.
	 (*print-level* nil) ; for sbcl.
	 (*package* *current-package*)) ; for ecl.
      (princ(situation<=index index))
      (loop(handler-case(prin1(DEBUG-EVAL(PROMPT index)index))
	     (skip-print()))))))

(defun situation<=index (index)
  (Nth-as-circle index *Acc*))

(defun subsituations(index &optional(situations *Acc*))
  (let((length(list-length situations)))
    (butlast ; Don't NBUTLAST!
      situations
      (-(1- length)
	(mod index length)))))

(eval-when(:load-toplevel :compile-toplevel :execute)
  (defmacro defop((name message)&body body)
    (check-type name keyword)
    (let((string(gensym "STRING")))
      `(let((,string ,message))
	 (CHECK-TYPE ,string STRING)
	 (SETF(GETHASH ,name *DEBUG-OPS*)
	   (CONS ,string (LAMBDA ,@body)))))))

(defop(:d (concatenate 'string "Move "(Red "D")"own stack frame, aiming bottom."))
      (index)
  (debug-repl(1+ index)))

(defop(:u (concatenate 'string "Move "(Red "U")"p stack frame, aiming top.")) (index)
  (debug-repl(1- index)))

(defop(:h (concatenate 'string (Red "H")"elp."))(index)
  (declare(ignore index))
  (maphash(lambda(k v)
	    (format t "~&~S~5T~A"k(car v)))
    *debug-ops*)
  (signal 'skip-print))

(defop(:q (concatenate 'string (Red "Q")"uit endaira repl, return to CL:debug loop."))
  (index)
  (declare(ignore index))
  #+ccl(throw 'Quit-signal (return-message))
  (signal 'Quit-signal))

(defun return-message()
  (format t "~&Return to CL:debug loop.~&Current bug is"))

(defop(:e1 (concatenate 'string "Do macro"(Red "E")"xpand-"(Red "1") "current stack frame form."))
  (index)
  (macroexpand-1(Form<=situation(situation<=index index))))

(defop(:e (concatenate 'string "Do macro" (Red "E") "xpand current stack frame form."))
  (index)
  (macroexpand(Form<=situation(situation<=index index))))

(defop(:s (concatenate 'string (Red "S")"ubstitute vars by arguments."))
  (index)
  (Substitute-sexp (Form<=situation(situation<=index index))
		   (make-environment index)))

(defun make-environment(index)
  (loop :for situation :in (nreverse(subsituations index))
	:for binds = (Binds<=situation situation)
	:append binds :into environment ; don't :NCONC!
	:nconc(make-list (length binds)
			 :initial-element(Name<=situation situation))
	:into names
	:finally (return (values environment names))))

(defop(:l (concatenate 'string "Show all "(Red "L")"ogs."))(index)
  (declare(ignore index))
  (mapc #'princ *Acc*)
  (signal 'skip-print))

(defop(:b (concatenate 'string "Show lexical "(Red "B")"indings."))(index)
  (let*((situation(situation<=index index))
	(binds(Binds<=situation situation)))
    (print-binds(set-prefix binds
			    (make-list (length binds)
				       :initial-element(Name<=situation situation))))
    (signal 'skip-print)))

(defun print-binds(binds)
  (let((num(reduce #'max binds :initial-value 0
		   :key (lambda(x)
			  (length(string(car x)))))))
    (dolist(bind binds)
      (apply #'uiop:format! t "~&~VS = ~S"num bind))))

(defop(:ab (concatenate 'string "Show "(Red "A")"ll "(Red "B")"indings."))(index)
  (print-binds(remove-duplicates(multiple-value-call #'set-prefix(make-environment index))
		:key #'car :test #'string=))
  (signal 'skip-print))

(defun set-prefix(binds names)
  (loop :for bind :in binds
	:for name :in names
	:collect(cons (format nil "~A/~S"name(car bind))
		      (cdr bind))))

(defop(:f (concatenate 'string "Show current stack "(Red "F")"rame."))(index)
  (Form<=situation(situation<=index index)))

(defop(:t (concatenate 'string (Red "T")"race function arguments."))(index)
  (declare(ignore index))
  (labels((NAME= (name situation)
		 (let((s-name(Name<=situation situation)))
		   (etypecase s-name
		     (symbol (eq s-name name))
		     (string (search(symbol-name name)s-name)))))
	  )
    (let((name(Prompt-for 'symbol "Function name?> ")))
      (loop :for situation :in *Acc*
	    :when (NAME= name situation)
	    :do (print-function-trace situation)))
    (signal 'skip-print)))

(defun print-function-trace(situation)
  (format t "Into> (~A ~{~S~^ ~})~%"
	  (Name<=situation situation)
	  (mapcar #'second (Binds<=situation situation))))

(defop(:ta (concatenate 'string (Red "T")"race "(Red "A")"ll function arguments."))
  (index)
  (declare(ignore index))
  (mapc #'print-function-trace *Acc*)
  (signal 'skip-print))

(defop(:pt (concatenate 'string (Red "P")"rint "(Red "T")"racing path."))(index)
  (declare(ignore index))
  (print-tracing)
  (signal 'skip-print))

(declaim (ftype (function (&optional stream)
			  (values null &optional))
		print-tracing))
(defun print-tracing(&optional(*standard-output* *debug-io*))
  (loop :initially (terpri)
	:for situation :in *Acc*
	:do (princ(Name<=situation situation))
	(write-string " => ")
	:finally (write-string "ERROR")))

(defvar *condition*)

(defop(:m (concatenate 'string "Print current condition "(Red "M")"essage."))
  (index)
  (declare(ignore index))
  (princ *condition*)
  (signal 'skip-print))
