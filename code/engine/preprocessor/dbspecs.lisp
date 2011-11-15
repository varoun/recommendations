;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-preprocessor)

;;; Initialising the various tables. It is an error to invoke these functions more than once on
;;; the same database.

(defun initialise-normalised-links-table (&optional (db-spec *database-spec*)
					  (table-name *normalised-links-table*)
					  (user-col *user-col*)
					  (item-col *item-col*))
  (with-database (pgsql db-spec :if-exists :old)
    (execute-command (format nil "create table ~a (~a integer, ~a integer)"
			     table-name user-col item-col)
		     :database pgsql)
    (execute-command (format nil "create index ~a on ~a (~a, ~a)"
			     (concatenate 'string table-name "_index")
			     table-name user-col item-col)
		     :database pgsql)))



(defun initialise-uid-map-table (&optional (db-spec *database-spec*)
				 (table-name *uid-map-table*)
				 (old-user-id *user-old-col*)
				 (new-user-id *user-new-col*))
  (with-database (pgsql db-spec :if-exists :old)
    (execute-command (format nil "create table ~a (~a integer, ~a integer)"
			     table-name old-user-id new-user-id)
		     :database pgsql)
    (execute-command (format nil "create index ~a on ~a (~a, ~a)"
			     (concatenate 'string table-name "_index")
			     table-name old-user-id new-user-id)
		     :database pgsql)))

(defun initialise-iid-map-table (&optional (db-spec *database-spec*)
				 (table-name *iid-map-table*)
				 (old-item-id *item-old-col*)
				 (new-item-id *item-new-col*))
  (with-database (pgsql db-spec :if-exists :old)
    (execute-command (format nil "create table ~a (~a integer, ~a integer)"
			     table-name old-item-id new-item-id)
		     :database pgsql)
    (execute-command (format nil "create index ~a on ~a (~a, ~a)"
			     (concatenate 'string table-name "_index")
			     table-name old-item-id new-item-id)
		     :database pgsql)))
