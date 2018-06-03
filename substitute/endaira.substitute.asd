; vim: ft=lisp et
(in-package :asdf)
(defsystem :endaira.substitute
  :depends-on
  (
   "lambda-list" ; tiny utilities for lambda list.
   )
  :components((:file "substitute-sexp")))
