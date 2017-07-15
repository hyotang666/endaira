(defpackage :endaira.copy(:use :cl)
  (:import-from :trestrul #:Mapleaf)
  (:export
    ;; main api
    #:copy

    ;; underlying helpers, but maybe useful
    #:copy-array
    #:copy-hash-table
    #:copy-pathname
    #:copy-obj
    ))
(in-package :endaira.copy)

(defun copy(arg)
  (typecase arg
    (list(Mapleaf #'copy arg))
    (atom (etypecase arg
	    (string(copy-seq arg))
	    (vector (map 'vector #'copy arg))
	    (array(copy-array arg))
	    (hash-table(copy-hash-table arg))
	    (readtable(copy-readtable arg))
	    (pathname(copy-pathname arg))
	    (random-state(make-random-state arg))
	    ((or symbol function character number stream package)
	     arg)
	    (t ; structure, class or condition.
	      (copy-obj arg))))))

(defun copy-array(array)
  (let((dest(make-array (array-dimensions array)
			:element-type (array-element-type array)
			:fill-pointer(array-has-fill-pointer-p array))))
    (dotimes(i (array-total-size array))
      (setf(row-major-aref dest i)(copy(row-major-aref array i))))
    dest))

(defun copy-hash-table(arg)
  (let((ht(make-hash-table :size(hash-table-size arg)
			   :test(hash-table-test arg)
			   :rehash-size(hash-table-rehash-size arg)
			   :rehash-threshold(hash-table-rehash-threshold arg))))
    (maphash(lambda(k v)
	      (setf(gethash k ht)(copy v)))
      arg)
    ht))

(defun copy-pathname(arg)
  #.(or #+sbcl 'ARG ; sbcl signals error especially compiled.
	`(make-pathname :device (copy(pathname-device arg))
			:directory(copy(pathname-directory arg))
			:host(copy(pathname-host arg))
			:name(copy(pathname-name arg))
			:type(copy(pathname-type arg))
			:version(copy(pathname-version arg)))))

(defun copy-obj(arg)
  (let((obj(if(eq 'structure-class(type-of(class-of arg)))
	    (copy-structure arg)
	    (closer-mop:class-prototype(find-class(type-of arg))))))
    (dolist(slot(slots<=obj arg))
      (when(slot-boundp arg slot)
	(setf(slot-value obj slot)(copy(slot-value arg slot)))))
    obj))

(defun slots<=obj(obj)
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots(class-of obj))))
