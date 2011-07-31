;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;;; Simple tests to verify that the compressed set retains the information present in the
;;;; original data.

(defgeneric build-set (source num-rows col)
  (:documentation 
"Build a set from the characteristic matrix or the signature matrix. The column number
represents the set, and the rows, elements from the universal set from which elements of the
set are drawn."))

(defgeneric similarity (source col1 col2 num-rows)
  (:documentation 
"Find the Jaccard similarity between two columns."))

(defmethod build-set ((source simple-array) num-rows col)
  (let ((result nil))
    (dotimes (index num-rows result)
      (push (aref source index col) result))))

;;; The jaccard similarity of two columns of the minhashed signature matrix.
(defmethod similarity ((source simple-array) col1 col2 num-hashfns)
  (let ((set1 (build-set source num-hashfns col1))
	(set2 (build-set source num-hashfns col2))
	(similar 0))
    ; the fraction of the rows that agree is an estimate of the Jaccard similarity.
    (loop 
       for item1 in set1
       for item2 in set2
       when (= item1 item2) do (incf similar))
    (float (/ similar num-hashfns))))
