;; vim: ft=lisp et
(defsystem :endaira
  :description "Common Lisp implementation especially for debuggin."
  :version "0.0.0"
  :author "Shinichi Sato <hyotang.karakoma@gmail.com>"
  :licence "LGPL"
  :depends-on
  (
   "structure-ext" ; structure extensions.
   "type-ext" ; type extensions.
   "prompt-for" ; type safe user input.
   "treat-as-circle" ; circle like sequence reference.
   "millet" ; wrapper for implementation dependent utilities.
   "lambda-list" ; tiny utilities for lambda list.
   "endaira.copy" ; recursive object copy.
   "endaira.substitute" ; substiture sexp.
   "cl-ansi-text" ; text colorizing.
   )
  :components((:module "internals"
                       :components 
                       ((:file "package")
                        (:file "specials":depends-on("package"))
                        (:file "condition":depends-on("package"))
                        (:file "situation":depends-on("package"))))
              (:module "dsl" :depends-on ("internals")
                       :components
                       ((:file "with-handler-named")))
              (:module "debugger" :depends-on ("internals")
                       :serial t
                       :components
                       ((:file "package")
                        (:file "debug-repl")
                        (:file "debugger")))
              (:module "builder" :depends-on ("internals" "dsl" "debugger")
                       :components
                       ((:file "package")
                        (:file "endaira-builder" :depends-on ("package"))))
              ))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "endaira"))))
  (append (call-next-method)'((test-op "endaira.test"))))
