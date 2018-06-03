; vim: ft=lisp et
(in-package :asdf)
(defsystem :endaira.copy
  :depends-on
  (
   "closer-mop" ; wrapper for meta object protocols.
   "trestrul" ; utilities for tree structured list.
   )
  :components((:file "copy")))
