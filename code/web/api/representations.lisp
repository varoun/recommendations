;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-


(in-package :groklogs-representations)

#|
The json format for "links" :
{
 "groklogsVersion":<version of groklogs>,
  "totalLinks":<the total number of links sent>,
  "links":[{"uid":<userid>,
            "link": [ <itemid>, <itemid>, <itemid> ]},
    	   {"uid":<userid>,
      	    "link": [ <itemid>, <itemid>, <itemid>, <itemid> ]}]}
|#

;;;; Protocol for parsing the json representations used in groklogs :

(defgeneric decode-representation (data data-format)
  (:documentation
"This function is the entrypoint for decoding external representations of resources to internal
  lisp structures. This generic function is intended to be specialised using the parameter
  data-format (json, xml ...)"))

;;;; Implementations of decode-groklogs-representation.

(defmethod decode-representation (data (data-format (eql 'json)))
  (let ((*parse-object-as-alist* t))
    (let* ((result (parse data))
	   (link-count (cdr (assoc "totalLinks" result :test #'equal)))
	   (version (cdr (assoc "groklogsVersion" result :test #'equal)))
	   (link-list (cdr (assoc "links" result :test #'equal))))
      (unless (= link-count (length link-list))
	(error "Link count does not match the number of links" link-count (length links)))
      ;; I should signal an error when any of link-count, version, links is nil!
      (loop 
	 for links in link-list
	 for uid = (cdr (assoc "uid" links :test #'equal))
	 for link = (cdr (assoc "link" links :test #'equal))
         collect (list uid link)))))
	   
	   
