(define (remove-dupls a-sentence)
	(if (> (count a-sentence) 1)
		(if (dupl-exist? (first a-sentence) (butfirst a-sentence))
			(remove-dupls (butfirst a-sentence))
			(sentence (first a-sentence) (remove-dupls (butfirst a-sentence))))
		a-sentence))
		
(define (dupl-exist? letter a-sentence)
	(member? letter a-sentence))
