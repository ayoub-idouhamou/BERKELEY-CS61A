
#lang scheme

(define (sum term a next b)
	(define (sum-accum a accum)
		(if (> a b)
			accum
			(sum-accum (next a) (+ accum (term a)))))
	(sum-accum a 0))

(define (num-integral func a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum func (+ a (/ dx 2)) add-dx b) dx))
;; to complete this procedure later
(define (simpson-integral func a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (* (/ h 3.0) (sum func a add-h b)))




