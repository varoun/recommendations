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

;;;; The keyword symbols.
(defvar *version-key* :GROKLOGS-VERSION)
(defvar *total-key* :TOTAL-LINKS)
(defvar *links-key* :LINKS)
(defvar *uid-key* :UID)
(defvar *link-key* :LINK)

;;;; Protocol for parsing the json representations used in groklogs :

(defgeneric decode-representation (data data-format)
  (:documentation
"This function is the entrypoint for decoding external representations of resources to internal
  lisp structures. This generic function is intended to be specialised using the parameter
  data-format (json, xml ...)"))

;;;; Implementations of decode-representation.

(defmethod decode-representation (data (data-format (eql 'json)))
  (let* ((result (decode-json data))
	 (link-count (cdr (assoc *total-key* result)))
	 (version (cdr (assoc *version-key* result)))
	 (link-list (cdr (assoc *links-key* result))))
    (unless (= link-count (length link-list))
      (error "Link count does not match the number of links" link-count (length link-list)))
    ;; I should signal an error when any of link-count, version, links is nil!
    (loop 
       for links in link-list
       for uid = (cdr (assoc *uid-key* links))
       for link = (cdr (assoc *link-key* links))
       collect (list uid link))))

;;;; Constructors that make the list that can be consumed by encode-json.

(defun make-version (version-number)
  (cons *version-key* version-number))

(defun make-linktotal (number-of-links)
  (cons *total-key* number-of-links))

(defun make-uid (uid)
  (cons *uid-key* uid))

(defun make-link (link)
  (if (listp link)
      (cons *link-key* link)
    (cons *link-key* (list link))))

(defun make-uid-link (uid-list link-list)
  (list uid-list link-list))

(defun make-link-representation (version total-links uid-link-list)
  (list version total-links (cons *links-key* uid-link-list)))
#|
GROKLOGS-REPRESENTATIONS 19 > (make-link-representation
                               (make-version 0.1)
                               (make-linktotal 2)
                               (list (make-uid-link 
                                      (make-uid 1)
                                      (make-link '(1 2 3)))
                                     (make-uid-link
                                      (make-uid 2)
                                      (make-link '(4 5 6)))))
((:GROKLOGS-VERSION . 0.1) (:TOTAL-LINKS . 2) (:LINKS ((:UID . 1) (:LINK 1 2 3)) ((:UID . 2) (:LINK 4 5 6))))

GROKLOGS-REPRESENTATIONS 20 > 
|#
