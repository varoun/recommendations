;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-similarity)

(defun expmod (base exp m)
  "Compute base raised to exp modulo m."
  (cond ((= exp 0) 1)
	((evenp exp)
	 (mod (expt (expmod base (/ exp 2) m) 2)
	      m))
	(t
	 (mod (* base (expmod base (1- exp) m))
	      m))))

(defun fermats-test (n)
  (labels ((try-it (a)
	     (= (expmod a n n) a)))
    (try-it (1+ (random (- n 1))))))

(defun fast-prime-p (n times)
  (do ((i 0 (1+ i)))
      ((= i times) t)
    (unless (fermats-test n)
      (return nil))))

(defun find-next-prime (n)
  "If a prime exists between n and (+ n 1000) return that, else return n."
  (do ((i n (1+ i)))
      ((> i (+ n 1000)) n)
    (when (fast-prime-p i 20)
      (return i))))
