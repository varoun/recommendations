;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

(defparameter *threshold* 0.5)

;;; Computing the band specification - the number of bands and rows per band.
(defun compute-band-spec (num-rows-sig-mat threshold)
  (labels ((compute-threshold (b r)
             (expt (/ 1 b) (/ 1 r))))
    (do* ((bands 1 (1+ bands))
          (rows num-rows-sig-mat (/ num-rows-sig-mat bands))
          (th '(1 bands rows)))
         ((= bands num-rows-sig-mat) th)
      (let ((thold (compute-threshold bands rows)))
        (when (< (abs (- threshold thold)) (first th)) ; delta between required and current.
          (setq th (list (abs (- threshold thold)) bands (float rows))))))))
#|
GROKLOGS-SIMILARITY> (compute-band-spec 64 .5)
(0.0 16 4.0)
GROKLOGS-SIMILARITY> 
|#

(defun hash-vector-to-bucket (bucket-array col-number col-vector hashfn)
  (push col-number
	(aref bucket-array
	      (hash-vector col-vector hashfn))))

(defun make-row-indices-for-band (start-index end-index max-rows)
  (loop 
     for rows from start-index to end-index while (<= rows max-rows) collect rows))
#|
GROKLOGS-SIMILARITY> (make-row-indices-for-band 1 10 10)
(1 2 3 4 5 6 7 8 9 10)
GROKLOGS-SIMILARITY> (make-row-indices-for-band 1 10 8)
(1 2 3 4 5 6 7 8)
GROKLOGS-SIMILARITY> 
|#

(defun lsh-sigmatrix (sigmatrix &optional (threshold *threshold*))
  (let* ((num-rows (1- (array-dimension sigmatrix 0)))
	 (num-columns (1-(array-dimension sigmatrix 1)))
	 (band-spec (compute-band-spec num-rows threshold))
	 (num-rows-per-band (floor (third band-spec)))
	 (num-buckets (find-next-prime num-columns))
	 (hashfn (make-double-hash num-buckets))
	 (result nil))
    (loop
       for band-start = 0 then (1+ (+ band-start num-rows-per-band))
       while (< band-start num-rows) do
	 (let ((bucket-array (make-array num-buckets :initial-element nil))
	       (band-index (make-row-indices-for-band band-start 
						      (+ band-start num-rows-per-band)
						      num-rows)))
	   (dotimes (col-index num-columns(push bucket-array result))
	     (let ((col-vector nil))
	       (dolist (band-row band-index)
		 (push (aref sigmatrix band-row col-index) col-vector))
	       (hash-vector-to-bucket bucket-array col-index col-vector hashfn)))))
    (setq lsh-result result)
    'done))
