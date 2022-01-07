

(define (calc)
  (display "calc: ")
  (flush)
  (print (calc-eval (read)))
  (calc))

(define (calc-eval exp)
  (cond ((number? exp) exp)
        ((list? exp) (calc-apply (car exp) (map calc-eval
                                                (cdr exp))))
        (else (error "calc: Bad expression: ", exp))))

(define (calc-apply func args)
  (cond ((equal? func '+) (accumulate + 0 args))
  		((equal? func '*) (accumulate * 1 args))
  		((equal? func '-) 
  			(cond ((null? args) (error "calc: Worng arguments: ", args))
  				  ((= (length args) 1) (- (car args)))
  				  (else (- (car args) (accumulate + 1 (cdr args))))))
		((equal? func '/) 
  			(cond ((null? args) (error "calc: Worng arguments: ", args))
  				  ((= (length args) 1) (/ (car args)))
  				  (else (/ (car args) (accumulate * 1 (cdr args))))))
		(else (error "calc: Bad operator: ", func))))
		
