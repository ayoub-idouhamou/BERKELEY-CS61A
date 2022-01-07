
(define (squares numbers)
	;takes set of numbers and returns their squares
	(cond ((empty? numbers) numbers)
			((= (count numbers) 1) (square (first numbers)))
			(else (sentence (square (first numbers)) (squares (butfirst numbers))))))

(define (square x)
	;return square of number
	(* x x))

(define (switch phrase)
	;takes a sentence as input
	;returns it switching 'me,'I by 'you
	;and 'you by 'I in begining of sentence, or by 'me otherwise
	
	(define (iterative-switch phrase begining?)
		(if (empty? phrase) 
			phrase
			(sentence (switch-word (first phrase) begining?) 
			(iterative-switch (butfirst phrase) false))))
			
	(define (switch-word a-word begining?)
		(cond ((member? a-word '(I me)) 'you)
				((and (equal? a-word 'you) begining?) 'I)
				((equal? a-word 'you) 'me)
				(else a-word)))
				
	(iterative-switch phrase true))
				
(define (ordered? numbers)
	;numbers: a sentence of numbers
	;returns true if numbers are in ascending order
	;false otherwise
	(cond	((empty? numbers) '(empty input check numbers set))
			((= (count numbers) 1) true)
			((> (first numbers) (first (butfirst numbers))) false)
			(else (ordered? (butfirst numbers)))))

(define (ends-e phrase)
	;;phrase: a sentence of words
	;;returns it only with words ending with e
	
	(define (word-ends-e a-word)
		(if (equal? (last a-word) 'e)
			a-word
			'()))
	 
	 (define (iter-check phrase)
	 	(if (empty? phrase) 
	 		'()
	 		(se (word-ends-e (first phrase)) (iter-check (bf phrase)))))
	 
	 (iter-check phrase))
	 	
			
			
	
	
	
	
	
	
	
