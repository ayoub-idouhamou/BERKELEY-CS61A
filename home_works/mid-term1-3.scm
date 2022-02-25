;; My attempts to answer the third sample of the first mid-term 

(load "simply")

;;an implementation of lg function, generates a recursive process
(define (lg n)
  (if (= n 1) 0 (+ 1 (lg (floor (/ n 2))))))

;; another solution for the lg function that generates
;; an iterative process

(define (lg n)
  (define (iter n result)
    (if (= n 1)
	result
	(iter (floor (/ n 2)) (+ result 1))))
  (iter n 0))
  
;; An implementation of the famous campfire game Mad-Libs
;; that uses a higher-order procedure, mad-libs in this case
;; you feed it a story in order to create another procedure
;; that works solely with the story you gave it
;; meaning it allows you to modify your story depending on the adjectives
;; and nouns lists you call it with
(define (mad-libs story)
  (lambda (nouns adjectives)
    (define (replace nouns adjectives a-story)
      (cond ((empty? a-story) '())
	    ((equal? (first a-story) '*NOUN)
	     (se (first nouns) (replace (bf nouns) adjectives (bf a-story))))
	    ((equal? (first a-story) '*ADJECTIVE)
	     (se (first adjectives) (replace nouns (bf adjectives) (bf a-story))))
	    (else (se (first a-story) (replace nouns adjectives (bf a-story))))))
    (replace nouns adjectives story)))


;; a procedure that transforms a sentence into a single word
(define (scrunch a-sentence)
  (if (null? a-sentence)
      ""
      (word (first a-sentence) (scrunch (bf a-sentence)))))

(define (word-maker template)
  (lambda (a-word)
    (scrunch (every (lambda (slot)
		      (if (equal? slot '*) a-word slot))
		    template))))

;; The association Abstract Data Type

;; constructors and selectors
(define make-association cons)
(define association-key car)
(define association-value cdr)

;; a procedure that feches an association from list of associations
;; given the key
(define (assoc key a-list)
  (cond ((null? a-list) false)
	((equal? (association-key (car a-list)) key) (car a-list))
	(else (assoc key (cdr a-list)))))

;;Create a "backwards" association from an old one
;; respecting the two abstract data types association and sentence
;; by using the API provided by every ADT
(define (index groups)
  (if (null? groups)
      '()
      (append (index-one (car groups)) (index (cdr groups)))))

(define (index-one group)
  (define (recur group-name people)
    (if (empty? people)
	'()
	(cons (make-association (first people) group-name)
	      (recur group-name (butfirst people)))))
  (recur (association-key  group) (association-value group)))
