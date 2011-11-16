;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-conf)

;;;; All database config details go here.

;;; Connection details.
(defparameter *database-host* "127.0.0.1")
(defparameter *database-name* "movielens")
(defparameter *database-user* "groky")
(defparameter *database-password* "groky")

(defparameter *database-spec* 
  (list *database-host* *database-name* *database-user* *database-password*))

;;; Tables.
(defparameter *primary-links-table* "links")
(defparameter *normalised-links-table* "links_normal")
(defparameter *uid-map-table* "uid_map")
(defparameter *iid-map-table* "iid_map")
(defparameter *related-table* "related_items")

;;; Columns

(defparameter *user-col* "userid")
(defparameter *item-col* "itemid")
(defparameter *item-old-col* "itemid_old")
(defparameter *item-new-col* "itemid_new")
(defparameter *user-old-col* "userid_old")
(defparameter *user-new-col* "userid_new")
(defparameter *related-item-col* "itemid_related")
(defparameter *score-col* "score")
