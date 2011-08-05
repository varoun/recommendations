;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-preprocessor)

;;; Initialising the various tables. It is an error to invoke these functions more than once on
;;; the same database.

(defun initialise-normalised-links-table (&optional (db-spec *database-spec*)
					  (table-name *normalised-links-table*))
  (with-database (pgsql db-spec :if-exists :old)
    (execute-command (format nil "create table ~a (userid integer, itemid integer)"
			     table-name)
		     :database pgsql)
    (execute-command (format nil "create index ~a on ~a (userid, itemid)"
			     (concatenate 'string table-name "_index")
			     table-name)
		     :database pgsql)))



(defun initialise-uid-map-table (&optional (db-spec *database-spec*)
				 (table-name *uid-map-table*))
  (with-database (pgsql db-spec :if-exists :old)
    (execute-command (format nil "create table ~a (userid_old integer, userid_new integer)"
			     table-name)
		     :database pgsql)
    (execute-command (format nil "create index ~a on ~a (userid_old, userid_new)"
			     (concatenate 'string table-name "_index")
			     table-name)
		     :database pgsql)))

(defun initialise-iid-map-table (&optional (db-spec *database-spec*)
				 (table-name *iid-map-table*))
  (with-database (pgsql db-spec :if-exists :old)
    (execute-command (format nil "create table ~a (itemid_old integer, itemid_new integer)"
			     table-name)
		     :database pgsql)
    (execute-command (format nil "create index ~a on ~a (itemid_old, itemid_new)"
			     (concatenate 'string table-name "_index")
			     table-name)
		     :database pgsql)))
