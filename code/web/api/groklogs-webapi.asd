;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :asdf)

(defsystem "groklogs-webapi"
  :description "The web services interface to the GrokLogs system."
  :version "0.0.1"
  :author "Varoun P <varoun@varoun.com>"
  :depends-on (:cl-ppcre 
	       :hunchentoot 
	       :cl-json
	       :clsql-postgresql-socket
	       :drakma)
  :serial t
  :components ((:file "../../conf/packages")
	       (:file "../../conf/conf-db")
	       (:file "packages")
	       (:file "dbspecs")
	       (:file "datastore")
	       (:file "representations")
	       (:file "client-rest")
	       (:file "rest-frame")))

