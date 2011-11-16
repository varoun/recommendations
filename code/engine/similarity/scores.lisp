;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

(defparameter *cutoff-threshold* 0.1
  "Ignore all elements with a similarity score lower than this.")

;;;; Simple tests to verify that the compressed set retains the information present in the
;;;; original data.

(defgeneric make-set (source field &optional field-type result-type db-spec source-links)
  (:documentation 
"Build a list representation of a set from the characteristic matrix or the signature matrix."))

(defgeneric similarity (source field1 field2 &optional field-type set-element-type)
  (:documentation 
"Find the Jaccard similarity between two columns."))

(defmethod make-set ((source simple-array) element-id &optional 
		     element-type 
		     result-type
		     db-spec
		     source-links)
  "Takes a signature matrix, and builds a list representation of the set of elements that make
up a column of the matrix."
  (declare (ignore element-type result-type db-spec source-links))
  (let ((result nil))
    (dotimes (index (array-dimension source 0) result)
      (push (aref source index element-id) result))))

(defmethod make-set ((source list) element-id &optional 
		     element-type 
		     result-type
		     (db-spec *database-spec*)
		     (source-links *normalised-links-table*))
  "Takes a characeristic matrix, as represented in the SQL DB, and builds a list representation
of the set of elements that are the members referred to by element-id."
  (with-database (sqldb db-spec :if-exists :new)
    (query (format nil "select ~a from ~a where ~a=~a"
		   result-type source-links element-type element-id)
	   :database sqldb :flatp t)))
#|
GROKLOGS-SIMILARITY> (make-set signature-matrix 17)
(0 9 2 1 4 4 11 13 18 0 18 7 3 1 4 1 4 1 1 3 1 0 3 8 1 10 0 2 2 17 1 6 2 3 10 0 5 5 3 4 4 5 0 9 0 5 7 4 0 0 2 4 0 11 6 1 3 0 0 0 1 2 21 8 9 8 10 1 0 2 7 2 4 2 4 2 2 5 0 7 1 1 6 4 0 3 3 12 9 2 3 0 6 1 5 4 3 0 1 0)
GROKLOGS-SIMILARITY> (make-set *database-spec* 1 "itemid" "userid")
... Output Deleted ...
|#

;;; The jaccard similarity of two columns of the minhashed signature matrix.
(defmethod similarity ((source simple-array) col1 col2 &optional col-type row-type)
  (declare (ignore col-type row-type))
  (let ((set1 (make-set source col1))
	(set2 (make-set source col2))
	(similar 0))
    ; the fraction of the rows that agree is an estimate of the Jaccard similarity.
    (loop 
       for item1 in set1
       for item2 in set2
       when (= item1 item2) do (incf similar))
    (float (/ similar (length set1))))) ; (= (length set1) (length set2)) ==> T

(defmethod similarity ((source list) field1 field2 &optional field-type result-type)
  (let ((set1 (make-set source field1 field-type result-type))
	(set2 (make-set source field2 field-type result-type)))
    (float (/ (length (intersection set1 set2))
	      (length (union set1 set2))))))
#|
GROKLOGS-SIMILARITY> (make-signature-matrix "userid" "itemid" 500)
DONE
GROKLOGS-SIMILARITY> (similarity signature-matrix 0 1)
0.088
GROKLOGS-SIMILARITY> (similarity *database-spec* 0 1 "itemid" "userid")
0.07180081
GROKLOGS-SIMILARITY> (similarity signature-matrix 17 20)
0.092
GROKLOGS-SIMILARITY> (similarity *database-spec* 17 20 "itemid" "userid")
0.11447811
GROKLOGS-SIMILARITY> 
|#

(defun find-similar-elements (element bucket-arrays sig-matrix)
  (let ((result nil))
    (loop for bucket-array in bucket-arrays do
	 (loop 
	    for bucket across bucket-array 
	    when (member element bucket) do
	      (let ((candidates (remove element bucket)))
		(dolist (candidate candidates)
		  (push `(,candidate ,(similarity sig-matrix element candidate))
			result)))))
    result))

;;; Map IDs to the original, and update the related_items table.
(defun find-original-id (id id-type id-map &optional 
			 (db-spec *database-spec*))
  (with-database (sqldb db-spec :if-exists :new)
    (first (query (format nil "select ~a from ~a where ~a=~a"
			  (concatenate 'string id-type "_old")
			  id-map
			  (concatenate 'string id-type "_new")
			  id)
		  :database sqldb :flatp t))))


(defun populate-related-items (bucket-arrays sig-matrix &optional
			       (db-spec *database-spec*)
			       (item-maps *iid-map-table*)
			       (related-items-table *related-table*)
			       (cutoff *cutoff-threshold*))
  (with-database (sqldb db-spec :if-exists :new)
    (dotimes (item (array-dimension sig-matrix 1))
      (loop for bucket-array in bucket-arrays do
	   (loop 
	      for bucket across bucket-array
	      when (member item bucket) do
		(let ((candidates (remove item bucket)))
		  (dolist (candidate candidates)
		    (let ((sim-score (similarity sig-matrix item candidate))
			  (item-orig (find-original-id item "itemid" item-maps db-spec))
			  (candidate-orig (find-original-id candidate "itemid" item-maps
							    db-spec)))
		      (when (> sim-score cutoff)
			(execute-command
			 (format nil "insert into ~a values (~a, ~a, ~a)"
				 related-items-table item-orig candidate-orig sim-score)
			 :database sqldb))))))))))
