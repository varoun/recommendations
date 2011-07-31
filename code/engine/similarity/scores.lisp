;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;;; Simple tests to verify that the compressed set retains the information present in the
;;;; original data.

(defgeneric make-set (source field &optional field-type result-type)
  (:documentation 
"Build a list representation of a set from the characteristic matrix or the signature matrix."))

(defgeneric similarity (source field1 field2 &optional field-type set-element-type)
  (:documentation 
"Find the Jaccard similarity between two columns."))

(defmethod make-set ((source simple-array) element-id &optional element-type result-type)
  "Takes a signature matrix, and builds a list representation of the set of elements that make
up a column of the matrix."
  (declare (ignore element-type result-type))
  (let ((result nil))
    (dotimes (index (array-dimension source 0) result)
      (push (aref source index element-id) result))))

(defmethod make-set ((source list) element-id &optional element-type result-type)
  "Takes a characeristic matrix, as represented in the SQL DB, and builds a list representation
of the set of elements that are the members referred to by element-id."
  (with-database (sqldb *database-spec* :if-exists :new)
    (query (format nil "select ~a from ~a where ~a=~a"
		   result-type *links-table-source* element-type element-id)
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
