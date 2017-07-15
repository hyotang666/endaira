; vim: ft=lisp et
(in-package :asdf)
(defsystem :endaira.substitute
  :depends-on(:lambda-list)
  :components((:file "substitute-sexp")))
