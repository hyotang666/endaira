; vim: ft=lisp et
(in-package :asdf)
(defsystem :endaira.substitute
  :version "0.0.0"
  :depends-on
  (
   "lambda-fiddle" ; tiny utilities for lambda list.
   )
  :components((:file "substitute-sexp")))
