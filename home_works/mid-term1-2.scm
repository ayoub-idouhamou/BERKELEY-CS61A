(define (every-nth n a-sentence)
  (define (loop counter rest)
    (cond ((null? rest) nil)
	  ((equal? (modulo counter n) 0)
	   (cons (car rest) (loop (+ 1 counter) (cdr rest))))
	  (else (loop (+ 1 counter) (cdr rest)))))
  (loop 1 a-sentence))
				  
