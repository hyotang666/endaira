(in-package :cl-user)

(defpackage :endaira.debugger(:use :cl :endaira.internals)
  (:import-from :endaira.substitute-sexp #:substitute-sexp)
  (:import-from :prompt-for #:Prompt-for)
  (:import-from :treat-as-circle #:Nth-as-circle)
  (:import-from :millet #:Function-name)
  (:import-from :cl-ansi-text #:Red)
  (:export
    ;;;; main api
    #:debugger-hook
    #:invoke-endaira-debugger
    ;;;; as dsl
    ;; definition
    #:defop
    ;; print-* family
    #:print-binds
    #:print-function-trace
    #:print-tracing
    ;; condition
    #:skip-print
    ;; repl
    #:debug-repl
    ;; helpers
    #:return-messaga
    #:situation<=index
    #:make-environment
    #:set-prefix
    ))
