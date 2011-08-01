;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

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
