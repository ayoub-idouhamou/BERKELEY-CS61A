(define (buzz number)
	(cond ((= (remainder number 7) 0) 'buzz)
		((member? 7 number) 'buzz)
		(else number)))
