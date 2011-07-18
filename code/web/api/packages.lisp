;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-


(in-package :cl-user)

;;; The package for dealing with representations.
(defpackage "GROKLOGS-REPRESENTATIONS" 
  (:use :cl :json)
  (:export "DECODE-REPRESENTATION"))

;;; The Core rest framework package.
(defpackage "GROKLOGS-REST" 
  (:use :cl :hunchentoot :cl-ppcre :groklogs-representations))
  
