(in-package :endaira.internals)

(defvar *acc* nil)

;;; ecl's debugger binds *package* with cl-user inside of debugger.
;;; we avoid it.
(defvar *current-package* *package*)

