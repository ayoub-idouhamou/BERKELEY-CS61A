<<<<<<< HEAD
=======
#lang scheme

>>>>>>> bc4b239 (add some files)
(define (product function a next b)
	;Computes product of applications of function on elements
	;over the range [a,b], next determines the next element
	(if (> a b)
		0
		(* (function a) (product function (next a) next b))))

<<<<<<< HEAD
(define (factorial x)
	(define (identity x) x)
	(define (increment x) (+ x 1))
	(product identity 1 increment x)

=======
>>>>>>> bc4b239 (add some files)




