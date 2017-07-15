; vim: ft=lisp et
(in-package :asdf)
(defsystem :endaira.test
  :depends-on
  (:jingoh "endaira")
  :components
  (#++(:file "endaira.core")
   (:file "endaira.debugger") (:file "endaira.dsl") (:file "endaira.internals"))
  :perform
  (test-op(o c)
    (symbol-call :jingoh :examine :endaira.debugger)
    (symbol-call :jingoh :examine :endaira.dsl)
    (symbol-call :jingoh :examine :endaira.internals)
    ))
