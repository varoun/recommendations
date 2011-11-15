;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-datastore)

;;;; Database schema.
(defun initialise-db (&optional (database-spec *database-spec*)
		      (table-name *primary-links-table*)
		      (users *user-col*)
		      (items *item-col*))
  (with-database (pgsql database-spec :if-exists :old)
    (execute-command 
     (format nil
	     "create table ~a (~a integer, ~a integer)"
	     table-name users items)
     :database pgsql)))
#|
GROKLOGS-DATASTORE 7 : 1 > (initialise-db)

GROKLOGS-DATASTORE 8 : 1 >
|#
