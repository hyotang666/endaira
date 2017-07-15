(in-package :endaira.internals)

(defstruct (situation(:copier nil)(:predicate nil))
  (name nil :type (OR SYMBOL STRING (CONS (EQL SETF) T)) :read-only t)
  (binds nil :type list :read-only t)
  (form nil :type t :read-only t))

;;; aliases
(Define-left-arrow-accessors situation
  name binds form)

;;; printer

(defmethod print-object((obj situation) *standard-output*)
  (if *print-escape*
    (call-next-method)
    (if(string= :break (name<=situation obj))
      (format t "~&Current binds are ~S"(binds<=situation obj))
      (format t "~&~%; Inside of ~A.~%~S"
	      (name<=situation obj)
	      (form<=situation obj)))))
