#lang scheme

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define integral
  (lambda (func a b dx)
    (* dx (sum func
               (+ a (/ dx 2))
               (lambda (x) (+ x dx))
               b))))

(define (simpson-integral func a b n)
  (define h (/ (- b a) n))
  (define yk (lambda (k) (func (+ a (* k h)))))
  (define simpson-term
    (lambda (k) (* (if (even? k) 2 4) (yk k)))) 
  (* (/ h 3)
     (+ (yk 0)
        (sum simpson-term 1 (lambda (k) (+ k 1)) (- n 1))
        (yk n))))

(define (product term a next b)
  (define (iter elem result)
    (if (> elem b)
        result
        (iter (next elem) (* (term elem) result))))
  (iter a 1))

(define factorial
  (lambda (n) (product (lambda (i) i)
                       1
                       (lambda (i) (+ i 1))
                       n)))

(define (accumulate combiner initial term a next b)
  (if (> a b)
      initial
      (combiner (term a)
                (accumulate combiner initial term
                            (next a) next b))))

(define (filtered-accumulate combiner initial predicate term a next b)
  (cond ((> a b) initial)
        (else (combiner (if (predicate a) (term a) initial)
                        (filtered-accumulate combiner
                                             initial
                                             predicate
                                             term
                                             (next a)
                                             next
                                             b)))))
                                        
                                        
(define product-nums-prime-to-n
  (lambda (n)
    (filtered-accumulate *
                         1
                         (lambda (i) (= (gcd i n) 1))
                         (lambda (i) i)
                         1
                         (lambda (i) (+ i 1))
                         (- n 1))))

(define prime?
  (lambda (n) (= (smallest-divisor n) n)))

(define smallest-divisor
  (lambda (n)
    (find-divisor n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define divides?
  (lambda (a b) (= (remainder b a) 0)))

(define square (lambda (x) (* x x)))
         
(define sum-squared-primes
  (lambda (a b)
    (filtered-accumulate +
                0
                prime?
                (lambda (i) i)
                a
                (lambda (i) (+ i 1))
                b)))

                       