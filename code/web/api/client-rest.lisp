;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-client)

;;; The resource URIs.
(defparameter *links-resource* "http://127.0.0.1:4242/1/groklogs/links")
(defparameter *user-resource* "http://127.0.0.1:4242/1/groklogs/user")
(defparameter *item-resource* "http://127.0.0.1:4242/1/groklogs/item")

;;; The protocol.
(defgeneric rest-request (resource-name uri http-method datatype &optional representation)
  (:documentation
"Make a REST API call to the URI using the specified HTTP method. The datatype specifies the
MIME type that the representation should use, typically JSON or XML."))

;;; Implementations of rest-request.
(defmethod rest-request ((resource-name (eql 'links)) uri (http-method (eql :PUT)) 
                         (datatype (eql 'json)) &optional representation)
  (http-request uri :method :put :content-length t 
                :content-type "application/json" :content representation))
#|
GROKLOGS-CLIENT 13 > (rest-request 'links *links-resource* :put 'json "{\"groklogsVersion\":0.1,\"totalLinks\":2,\"links\":[{\"uid\":1,\"link\":[1,2,3]},{\"uid\":2,\"link\":[4,5,6]}]}")
""
200
((:DATE . "Thu, 21 Jul 2011 06:37:10 GMT") (:SERVER . "Hunchentoot 1.1.1") (:CONNECTION . "Close") (:CONTENT-TYPE . "text/html; charset=iso-8859-1"))
#<PURI:URI http://localhost:4242/1/groklogs/links>
#<FLEXI-STREAMS:FLEXI-IO-STREAM 22A4EBCB>
T
"OK"

GROKLOGS-CLIENT 14 > 
|#
