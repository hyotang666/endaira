(in-package :cl-user)

(defpackage :endaira.core (:use)
  (:export 
    ;;;; for global
    #:defun #:defmacro #:defmethod
    ;;;; for local
    #:flet #:labels #:macrolet
    ;;;; for bindings
    #:let #:let* #:multiple-value-bind #:destructuring-bind #:dolist
    ;;;; for diverging
    #:cond #:case #:ecase #:ccase #:typecase #:etypecase #:ctypecase
    ;;;; avoid side effect
    #:ignore-errors #:handler-case
    ;;;; for debug
    #:break
    ))

(defpackage :endaira (:use :endaira.core)
  (:import-from
    :cl . #.(loop :for symbol :being :each :external-symbol :in :cl
		  :unless(find-symbol(symbol-name symbol):endaira.core)
		  :collect symbol))
  (:import-from :endaira.dsl #:*copy*)
  (:export . #.(loop :for symbol :being :each :external-symbol :in :cl
		     :collect symbol)))
