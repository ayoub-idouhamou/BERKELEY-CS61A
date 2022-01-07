(define (plural wd)
	(if (is-special-case? wd)
		(word (butlast wd) 'ies)
		(word wd 's)))

(define (vowel? letter)
	(member? letter '(a e u i o)))

(define (is-special-case? wd)
	(and (not (vowel? (last (butlast wd)))) (equal? (last wd) 'y)))
	

