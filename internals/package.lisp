(in-package :cl-user)
(defpackage :endaira.internals(:use :cl)
  (:import-from :structure-ext #:Define-left-arrow-accessors)
  (:export
    ;;;; specials
    #:*acc*
    #:*current-package*
    ;;;; condition
    #:quit-signal
    ;;;; situation object
    #:situation ; type name
    #:make-situation ; constructor
    ;; accessors
    ;; formal-name	;; alias
    #:situation-name	#:name<=situation
    #:situation-binds	#:binds<=situation
    #:situation-form	#:form<=situation
    ))
