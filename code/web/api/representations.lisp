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

;;;; Implementations of decode-representation.

(defmethod decode-representation (data (data-format (eql 'json)))
  (let* ((result (decode-json data))
	 (link-count (cdr (assoc :TOTAL-LINKS result)))
	 (version (cdr (assoc :GROKLOGS-VERSION result)))
	 (link-list (cdr (assoc :LINKS result))))
    (unless (= link-count (length link-list))
      (error "Link count does not match the number of links" link-count (length link-list)))
    ;; I should signal an error when any of link-count, version, links is nil!
    (loop 
       for links in link-list
       for uid = (cdr (assoc :UID links))
       for link = (cdr (assoc :LINK links))
       collect (list uid link))))
