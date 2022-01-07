(define (square x)
	(* x x))
(define (sum-squares x y)
	(+ (square x) (square y)))
	
(define (sum-squares-of-larg-nums a b c)
	(cond ((and (>= a b) (>= b c)) (sum-squares a b))
	      ((and (<= a b) (<= b c)) (sum-squares b c))
	      ((and (>= a b) (<= b c)) (sum-squares a c)))) 
