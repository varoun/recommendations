;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

;;; We use double double hashing to generate 'random' permutations of rows.
;;; Note: The number-of-buckets parameter needs to be prime for the permutations to be random,
;;; i.e not collide.

(defun make-h1 (number-of-buckets)
  #'(lambda (key) (mod key number-of-buckets)))

(defun make-h2 (number-of-buckets)
  #'(lambda (key) (+ 1 (mod key (- number-of-buckets 1)))))

(defun make-double-hash (number-of-buckets)
 (let ((f1 (make-h1 number-of-buckets))
       (f2 (make-h2 number-of-buckets)))
   #'(lambda (k i)
       (mod (+ (funcall f1  k)
               (* i (funcall f2 k)))
            number-of-buckets))))
#|
GROKLOGS-SIMILARITY> (setq dh (make-double-hash 701))
#<CCL:COMPILED-LEXICAL-CLOSURE (:INTERNAL MAKE-DOUBLE-HASH) #x18A36D46>
GROKLOGS-SIMILARITY> (funcall dh 123456 1)
337
GROKLOGS-SIMILARITY> (funcall dh 123456 0)
80
GROKLOGS-SIMILARITY> 
|#
