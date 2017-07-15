; vim: ft=lisp et
(in-package :asdf)
(defsystem :endaira.copy
  :depends-on (:closer-mop :trestrul)
  :components((:file "copy")))
