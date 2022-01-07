#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sign x)
  (if (>= x 0) + -))

(define (make-rat n d)
  (define g (abs (gcd n d)))
  (cons ((sign (* n d)) (abs (/ n g))) (abs (/ d g))))

(define numer car)
(define denom cdr)

(define (add x y)
  (make-rat (+ (* (numer x) (denom y)) (* (denom x) (numer y)))
            (* (denom x) (denom y))))




