;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defsystem groklogs-webapi
  (:default-pathname "./")
  :members ("packages"
            "dbspecs"
            "datastore" 
            "representations"
            "client-rest"
            "rest-frame"))
