#lang scheme
;Procedure implelemeting the general notion of summing results for applying
;;a function over a given interval
(define (sum func a next b)
  (if (> a b)
      0
      (+ (func a) (sum func (next a) next b))))
;;examles of use cases
(define sum-squares
  (lambda (a b) (sum (lambda (x) (* x x))
                     a
                     (lambda (x) (+ x 1))
                     b)))
(define sum-integers
  (lambda (a b) (sum (lambda (x) x)
                     a
                     (lambda (x) (+ x 1))
                     b)))

;EX 1.29 Numerical integration
;When it comes to calculating numerical integrals, Simpson's rule is considered a
;more accurate method, compared to some other counterparts
;;let us begin with a less accurate one                 
(define integral
  (lambda (func a b dx)
    (* (sum func
            (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b) dx)))

(define (simpson-integral func a b n)
  (define h (/ (- b a) n))
  (define yk (lambda (k) (func (+ a (* k h)))))
  (define simpson-term
    (lambda (k) (* (if (even? k) 2.0 4.0) (yk k)))) 
  (* (/ h 3.0)
     (+ (yk 0)
        (sum simpson-term 1 (lambda (k) (+ k 1)) (- n 1))
        (yk n))))
;;Computing the inegral of the function f(x) = x*x*x
;;using the simpson Rule gives a more accurate result .25
;;I have also noticed that after only 4 iteration, the integral
;;converges to our desired result meaning using a value n=4

;EX 1.30 An iterative version of sum
(define (sum-iter func a next b)
  (define (iter counter result)
    (if (> counter b)
        result
        (iter (next counter) (+ (func counter) result))))
  (iter a 0))

;EX 1.31
; a- An analogous procedure of sum, preforming product instead
;of summing, It differs only in the zero value and the combination
;operation. Let us make it an iterative one. 
(define (product-iter func a next b)
  (define (iter elem result)
    (if (> elem b)
        result
        (iter (next elem) (* (func elem) result))))
  (iter a 1))
;The recursive version
(define (product func a next b)
  (if (> a b) 1 (* (func a) (product func (next a) next b))))
;We can express factorial straightforwardly in terms of product
;like this
(define factorial
  (lambda (n) (product (lambda (i) i)
                       1
                       (lambda (i) (+ i 1))
                       n)))
;We could also use this higher-order prodcedure product to implement
;the mathematical formula for approximating PI, discovered by
;the English mathematicien John Wallis
(define pi-term
    (lambda (k) (/ (* 2 k (+ (* 2 k) 2))
                   (square (+ (* 2 k) 1)))))
(define square (lambda (x) (* x x)))
(define (approximate-PI n)
  (* 4.0 (product pi-term 1 (lambda (k) (+ k 1)) n)))

;EX 1.32 It turns out, sum and product are special cases of still
;more general principle called accumulation, that consists of
;combining collections of values using some special combination
;operation
;So for us to capture this general pattern of combinig data,
;we have to factor out the operation of combination and the zero
;value(0 for adding, 1 for multiplying)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term
                            (next a) next b))))
;b - One that generates an iterative process
(define accumulate-iter
  (lambda (combiner null-value term a next b)
    (define (iter elem accumul)
      (if (> elem b)
          accumul
          (iter (next elem) (combiner (term elem) accumul))))
    (iter a null-value)))

;EX 1.33 Now we gonna implement the filtered accumulate procedure
;by introducing the notion of filtering.
;Basicaly, it says combilne only the terms resulting from applying
;the function on points over the given range that satisfy a
;a specified condition, it takes the same arguments as accumulate
;plus the predicate argument that specifies the condition.
(define (filtered-accumulate combiner null-value predicate term a next b)
  (cond ((> a b) null-value)
        (else (combiner (if (predicate a) (term a) null-value)
                        (filtered-accumulate combiner
                                             null-value
                                             predicate
                                             term
                                             (next a)
                                             next
                                             b)))))
;a- Let's use filter-accumulate proc to impelement another procedure
;that computes the squares of the primes numbers existing in the
;interval [a, b]
(define squared-primes-sum
  (lambda (a b)
    (filtered-accumulate + 0 prime? (lambda (n) (* n n))
                         a
                         (lambda (n) (+ n 1))
                         b)))
;;A procedure that verifies wether a number is prime
(define (prime? n)
  (define smallest-divisor
    (lambda (n)
      (find-divisor n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define divides?
    (lambda (a b) (= (remainder b a) 0)))
  (= (smallest-divisor n) n))
;b -The product of all positive intergers less than n that are
;relatively primes to n
(define product-primes-to-n
  (lambda (n)
    (filtered-accumulate *
                         1
                         (lambda (i) (= (gcd i n) 1))
                         (lambda (i) i)
                         1
                         (lambda (i) (+ i 1))
                         (- n 1))))                       