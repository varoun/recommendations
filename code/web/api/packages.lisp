;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-


(in-package :cl-user)

;;; The datastore.
(defpackage "GROKLOGS-DATASTORE"
  (:use :cl :clsql :groklogs-conf)
  (:export "ADD-TO-LINKS"
	   "GET-RELATED-ITEMS"))


;;; The package for dealing with representations.
(defpackage "GROKLOGS-REPRESENTATIONS" 
  (:use :cl :json :groklogs-conf)
  (:export 
   "DECODE-REPRESENTATION"
   "MAKE-VERSION"
   "MAKE-LINKTOTAL"
   "MAKE-UID"
   "MAKE-LINK"
   "MAKE-UID-LINK"
   "MAKE-LINK-REPRESENTATION"
   "ENCODE-REPRESENTATION"
   "MAKE-RELATED-REPRESENTATION"))
   

;;; The Core rest framework package.
(defpackage "GROKLOGS-REST" 
  (:use :cl :hunchentoot 
	:cl-ppcre :groklogs-representations 
	:groklogs-datastore :groklogs-conf))

;;; The REST client.
(defpackage "GROKLOGS-CLIENT" 
  (:use :cl :drakma :cl-ppcre :groklogs-representations :groklogs-conf)
  (:export "REST-REQUEST"))

