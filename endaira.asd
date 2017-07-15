;; vim: ft=lisp et
(defsystem :endaira
  :description "Common Lisp implementation especially for debuggin."
  :version "0.0.0"
  :author "Shinichi Sato <hyotang.karakoma@gmail.com>"
  :licence "LGPL"
  :depends-on(:structure-ext :type-ext :prompt-for :treat-as-circle :millet :lambda-list :endaira.copy :endaira.substitute "cl-ansi-text")
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
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "endaira"))))
  (test-system :endaira.test))
