;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;; creating the related table.
(defun initialise-related-items-table (&optional (dbspec *database-spec*)
				       (table-name *related-table*)
				       (items *item-col*)
				       (related-items *related-item-col*)
				       (score *score-col*))
  (with-database (sqldb dbspec :if-exists :new)
    (execute-command 
     (format nil 
	     "create table ~a (~a integer, ~a integer, ~a float)"
	     table-name items related-items score)
     :database sqldb)
    (execute-command (format nil "create index ~a on ~a (~a, ~a)"
			     (concatenate 'string table-name "_index")
			     table-name items related-items)
		     :database sqldb)))
