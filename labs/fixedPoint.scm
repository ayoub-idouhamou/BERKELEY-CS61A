(define tolerance 0.00001)

(define (find-fixed-point function first-guess)
	(define (is-close-enough? value1 value2)
		(< (abs (- value1 value2)) tolerance))
		
	(define (try guess)
		(let ((next (function guess)))
			(if (is-close-enough? guess next)
				next
				(try next))))
				
	(try first-guess))

(define (average x y)
	(/ (+ x y) 2))
	
(define (square-root x)
	(find-fixed-point (lambda (y) (average y (/ x y))) 1.0))


