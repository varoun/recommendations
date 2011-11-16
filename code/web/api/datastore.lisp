;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-datastore)

(defun add-to-links (alist-of-links &optional
		     (db-spec *database-spec*)
		     (links-table *primary-links-table*))
  (with-database (pgsql db-spec :if-exists :old)
    (loop 
     for link in alist-of-links
     for userid = (first link)
     for items = (second link) do
     (loop for itemid in items do
           (execute-command
            (format nil "insert into ~a values (~a, ~a)" links-table userid itemid)
            :database pgsql)))))

(defun get-related-items (item &optional
			  (db-spec *database-spec*)
			  (item-col *item-col*)
			  (related-items *related-item-col*)
			  (related-items-table *related-table*)
			  (score *score-col*))
  (with-database (sqldb db-spec :if-exists :new)
    (mapcar #'first
	    (query (format nil
			   "select distinct ~a, ~a from ~a where ~a=~a order by ~a desc" 
			   related-items score related-items-table item-col item score)
		   :database sqldb :flatp t))))
