(define (search function negative-point positive-point)
	(let ((midpoint (average negative-point positive-point)))
		(if (close-enough? positive-point negative-point)
			midpoint
			(let ((test-value (function midpoint)))
				(cond ((positive? test-value)
					(search function negative-point midpoint))
				      ((negative? test-value)
				      	(search function midpoint positive-point))
				      (else midpoint))))))


(define (average x y)(/ (+ x y) 2))
	
(define (close-enough? a b)(< (abs (- a b)) 0.000001))

(define (half-interval-search function a b)
	(let ((a-value (function a))
	      (b-value (function b)))
	  (cond ((and (negative? a-value) (positive? b-value))
	  		(search function a b))
	  	((and (positive? a-value) (negative? b-value))
	  		(search function b a))
	  	(else (error "The values are not of opposite sign" a b)))))
	



