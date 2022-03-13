(load "simply")

;; An implementation for the Bridge card game

;; constructor and selectors of the ADT card
(define suit car)
(define rank cdr)
(define (make-card a-suit a-rank) (cons a-suit a-rank)) 

;; returns the hand worth value in order to bid or not
(define (hand-value a-hand)
  (+ (high-card-points a-hand) (hand-dist-points a-hand)))

;; calculates the total points of the highest ranks
(define (high-card-points a-hand)
  (reduce + (map card-value a-hand)))

;; returns the value of a card considering rank
(define (card-value a-card)
  (let ((a-rank (rank a-card)))
    (cond ((eq? a-rank 'Ace) 4)
	  ((eq? a-rank 'King) 3)
	  ((eq? a-rank 'Queen) 2)
	  ((eq? a-rank 'Jack) 1)
	  (else 0))))

;; computes the suit distribution points of a hand 
(define (hand-dist-points a-hand)
  (reduce + (map suit-dist-points (suit-counts a-hand))))

;; calculates the suit dist. points for a particular suit
(define (suit-dist-points suit-count)
  (cond ((= suit-count 0) 3)
	((= suit-count 1) 2)
	((= suit-count 2) 1)
	(else 0)))

;; returns a list containing the number of cards for every suit
(define (suit-counts a-hand)
  (define count (lambda (a-suit) (count-suit a-suit a-hand)))
  (map count '(Spade Heart Club Diamond)))

;; counts the nb. of cards for a particular suit 
(define count-suit
  (lambda (a-suit a-hand)
    (length (filter (lambda (a-card) (eq? (suit a-card) a-suit)) a-hand))))


