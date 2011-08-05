;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-conf)

;;;; All database config details go here.

;;; Connection details.
(defparameter *database-host* "127.0.0.1")
(defparameter *database-name* "groklogs")
(defparameter *database-user* "groky")
(defparameter *database-password* "groky")

(defparameter *database-spec* 
  (list *database-host* *database-name* *database-user* *database-password*))

;;; Tables.
(defparameter *primary-links-table* "links")
(defparameter *normalised-links-table* "links_normal")
(defparameter *uid-map-table* "uid_map")
(defparameter *iid-map-table* "iid_map")
(defparameter *links-table-source* "links_normal")
(defparameter *related-table* "related_items")
