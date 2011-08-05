;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;; creating the related table.
(defun initialise-related-items-table (&optional (dbspec *database-spec*)
				       (table-name *related-table*))
  (with-database (sqldb dbspec :if-exists :new)
    (execute-command 
     (format nil 
	     "create table ~a (itemid integer, itemid_related integer, score float)"
	     table-name)
     :database sqldb)
    (execute-command (format nil "create index ~a on ~a (itemid, itemid_related)"
			     (concatenate 'string table-name "_index")
			     table-name)
		     :database sqldb)))
