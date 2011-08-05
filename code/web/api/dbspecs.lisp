;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-datastore)

;;;; Database schema.
(defun initialise-db ()
  (with-database (pgsql *database-spec* :if-exists :old)
    (execute-command 
     "create table links (userid integer, itemid integer)"
     :database pgsql)))
#|
GROKLOGS-DATASTORE 7 : 1 > (initialise-db)

GROKLOGS-DATASTORE 8 : 1 >
|#
