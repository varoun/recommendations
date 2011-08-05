;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage "GROKLOGS-CONF"
  (:use :cl)
  (:export "*DATABASE-HOST*"
	   "*DATABASE-NAME*"
	   "*DATABASE-USER*"
	   "*DATABASE-PASSWORD*"
	   "*DATABASE-SPEC*"
	   "*PRIMARY-LINKS-TABLE*"
	   "*NORMALISED-LINKS-TABLE*"
	   "*UID-MAP-TABLE*"
	   "*IID-MAP-TABLE*"
	   "*LINKS-TABLE-SOURCE*"
	   "*RELATED-TABLE*"))