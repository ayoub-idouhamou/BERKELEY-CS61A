
(define (filtered-accumulate combiner null-value term predicate a next b )
	(cond ((> a b) null-value)
	      ((predicate a) (combiner (term a) (filtered-accumulate 
	      combiner null-value term predicate (next a) next b)))
	      (else (filtered-accumulate combiner null-value term predicate
	      (next a) next b))))

(define (prime? number)
	(define (check-divisor number num)
		(cond ((< number 2) false)
			((= number num) true)
			((= (remainder number num) 0) false)
			(else (check-divisor number (+ num 1)))))
	(check-divisor number 2))

(define (sum-primes-squared start end)
	;; computes sum of squares of prime numbers
	;;over interval [start,end]
	(filtered-accumulate + 0 square prime? start increment end))

(define (product-prime-numbers n)
	(filtered-accumulate * 1 identity prime? 2 increment (- n 1)))

(define (increment x) (+ x 1))


(define (identity x) x)
	      
(define (sum term a next b)
	(accumulate + 0 term a next b))
	
(define (product term a next b)
	;Computes product of terms
	;over the range [a,b], next determines the next element
	(accumulate * 1 term  a next b))

(define (factorial x)
	(define (identity x) x)
	(define (increment x) (+ x 1))
	(product identity 1 increment x))

(define (square x)
	(* x x))

(define (approximate-pi n)
	(define (term x)
		(/ (* x (+ x 2)) (square (+ x 1))))
	(define (increment-2 x) (+ x 2))
	(* 4 (product term 2 increment-2 n)))

(define (compose f g)
	(lambda (x) (f (g x))))
	
(define (repeated func n)
	(define (repeated-compose n)
		(if (< n 1)
			func
			(compose (repeated-compose (- n 1)) func)))
	(repeated-compose n))

(define dx 0.000001)
(define (smooth function)
	(define (average x y z)
		(/ (+ x y z) 3))
	(lambda (x) (average (function (- x dx))                         
			     (function x)
			     (function (+ x dx)))))
		
(define (n-fold function n)
	((repeated smooth n) function))


(define tolerance 0.000001)
(define (fixed-point function first-guess)
	(define (close-enough? value1 value2)
		(< (abs (- value2 value1)) tolerance))
	(define (try guess)
		(let ((next (function guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (average-damp function)
	(define (average x y) (/ (+ x y) 2))
	(lambda (x) (average x (function x))))

(define (square-root x)
	(fixed-point (average-damp (lambda (y) (/ x y)))
				  1.0))


(define (every function a-sentence)
	(if (empty? a-sentence)
		'()
		(sentence (function (first a-sentence))
				   (every function (butfirst a-sentence)))))
				





























	
	






