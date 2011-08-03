;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-datastore)

(defun add-to-links (alist-of-links)
  (with-database (pgsql *database-spec* :if-exists :old)
    (loop 
     for link in alist-of-links
     for userid = (first link)
     for items = (second link) do
     (loop for itemid in items do
           (execute-command
            (format nil "insert into links values (~a, ~a)" userid itemid)
            :database pgsql)))))

(defun get-related-items (item)
  (with-database (sqldb *database-spec* :if-exists :new)
    (query (format nil
		   "select itemid_related from related_items where itemid=~a and score > 0.15
  order by score desc" item)
	   :database sqldb :flatp t)))
