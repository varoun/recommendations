;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;;; Minhash the characteristic matrix, as represented by the userid/itemid database table.

;;; A suitably large constant to serve as infinity.
(defconstant +infinity+ most-positive-fixnum)

;;; The hash function needed to randomly permute rows of the characteristic matrix. We need to
;;; make the number-of-buckets used to initialise the function prime.
(defvar *random-hash*)

;;; The signature matrix - 
(defvar *sig*)

;;; When we call the functions that need to know the max users/items, we use find-max-element.
(defun find-max-element (element-name)
  (with-database (sqldb *database-spec* :if-exists :new)
    (first (query (format nil "select max(~a) from ~a" element-name *links-table-source*)
		  :database sqldb :flatp t))))

;;; Updating a column of the signature matrix with a new column vector.
(defun update-signature-matrix (matrix col-num new-col)
  (dotimes (row (length new-col))
    (when (< (elt new-col row)
             (aref matrix row col-num))
      (setf (aref matrix row col-num) (elt new-col row)))))

;;; Construct a column vector by applying the hash-function.
;;; Note: Incrementing the index below ensures that the first hash function on each row does
;;; not produce the same row number.
(defun make-hash-column (row hashfn size)
  (let ((col (make-array size)))
    (dotimes (index size)
      (setf (elt col index) 
	    (funcall hashfn row (1+ index))))
    col))

;;; Building the signature matrix.
(defun make-signature-matrix (row-type col-type num-hash)
  (let* ((rows (find-max-element row-type))
	 (cols (find-max-element col-type))
	 (*sig* (make-array `(,num-hash ,(1+ cols)) :initial-element +infinity+))
	 (*random-hash* (make-double-hash (find-next-prime rows))))
    (with-database (sqldb *database-spec* :if-exists :new)
      (dotimes (index (1+ rows))
	(let ((row-elements (query (format nil 
					   "select ~a from ~a where ~a=~a"
					   col-type *links-table-source* row-type index)
				   :flatp t :database sqldb))
	      (hash-vector (make-hash-column index *random-hash* num-hash)))
	  (dolist (col-index row-elements)
	    (update-signature-matrix *sig* col-index hash-vector)))))
    (setq signature-matrix *sig*)
    'done))
