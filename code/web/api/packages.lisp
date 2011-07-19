;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-


(in-package :cl-user)

;;; The datastore.
(defpackage "GROKLOGS-DATASTORE"
  (:use :cl :clsql :clsql-postgresql-socket)
  (:export "ADD-TO-LINKS"))


;;; The package for dealing with representations.
(defpackage "GROKLOGS-REPRESENTATIONS" 
  (:use :cl :json)
  (:export "DECODE-REPRESENTATION"))

;;; The Core rest framework package.
(defpackage "GROKLOGS-REST" 
  (:use :cl :hunchentoot :cl-ppcre :groklogs-representations :groklogs-datastore))
  
