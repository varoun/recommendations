;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-rest)

;;;; REGEX for item and user dispatchers, version 1.
;; Note: Remove case sensitivity in these.
(defparameter *api-regex* "^/[0-9]/[a-z]+/(\\buser\\b|\\bitem\\b)/[0-9]+") 

(defparameter *link-regex* "^/[0-9]/[a-z]+/(\\blinks\\b)")

(defun setup-api-regex-dispatcher ()
  (push (create-regex-dispatcher *api-regex* 'api-handler)
	*dispatch-table*)
  (push (create-regex-dispatcher *link-regex* 'api-handler)
        *dispatch-table*)
  'done)

(setup-api-regex-dispatcher)

;;;; The handlers.
(defun api-handler ()
  (let* ((http-method (request-method*))
	 (uri-components (split "/" (request-uri*)))
	 (api-version (parse-integer (second uri-components)))
	 (client (third uri-components))
	 (resource (fourth uri-components)))
    (process-api api-version
                 (intern resource :groklogs-rest)
		 http-method
                 client)))


;;;; The protocol
(defgeneric process-api (api-version resource http-method client)
  (:documentation
"Take an API version, client name, a resource, and the http-method and build a response."))

;;;; The implementation for process-api.
(defmethod process-api ((api-version (eql 1)) (resource (eql '|user|)) 
                        (http-method (eql :GET)) client)
  (format nil "API Version: ~a, Client: ~a, Resource: ~a, Method: ~a"
	  api-version
	  client
	  resource
	  http-method))

(defmethod process-api (api-version resource http-method client)
  (format nil "DEFAULTING -- API Version: ~a, Client: ~a, Resource: ~a, Method: ~a"
	  api-version
	  client
	  resource
	  http-method))

(defmethod process-api ((api-version (eql 1)) (resource (eql '|links|)) 
                        (http-method (eql :PUT)) client)
  (let ((http-body (raw-post-data :want-stream t)))
    (prin1-to-string (decode-representation http-body 
                                            (intern "JSON" :groklogs-representations)))))
