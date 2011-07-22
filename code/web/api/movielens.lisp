;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-client)

(defun make-movielens-uid-items (uid-items)
  (make-uid-link (make-uid (first uid-items))
                 (make-link (rest uid-items))))

(defun make-json-representation (uid-item)
  (encode-representation
   (make-link-representation (make-version 0.1)
                             (make-linktotal 1)
                             (list (make-uid-link
                                    (make-uid (first uid-item))
                                    (make-link (rest uid-item)))))
   (intern "JSON" :groklogs-representations)))

(defun send-movielens-data (filestream)
  (loop 
   for uid-item = (read filestream nil nil) while uid-item do
   (rest-request 'links *links-resource* :put 'json (make-json-representation uid-item))))
