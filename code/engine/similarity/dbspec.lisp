;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;; Database connection details go here.
(defparameter *database-spec* (list "127.0.0.1" "groklogs" "groky" "groky"))

;;; The various tables.
(defparameter *links-table-source* "links_normal")
