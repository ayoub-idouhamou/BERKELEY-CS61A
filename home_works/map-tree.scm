
;;Exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
	((list? tree)
	 (cons (square-tree (car tree)) (square-tree (cdr tree))))
	(else (* tree tree))))

;;A version using map of list
(define (square-map-tree tree)
  (map (lambda (sub-tree)
	 (if (list? sub-tree)
	     (square-map-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))

;;Exercise 2.31
;;An abstraction of (map func list)
(define (map func tree)
  (cond ((null? tree) nil)
	((list? tree)
	 (cons (map func (car tree)) (map (cdr tree))))
	(else (func tree))))

;;Exercise 2.32: Generate set of all subsets of a set
(define (subsets set)
  (if (null? set)
      (list nil)
      (let ((rest (subsets (cdr set))))
	(append rest (map (lambda (subset)
			    (cons (car set) subset))
			  rest)))))

;; An implementation of tree filter
(define (filter predicate tree)
  (cond ((null? tree) nil)
	((list? tree) (cons (filter predicate (car tree))
			    (filter predicate (cdr tree))))
	((predicate tree) tree)))

(define (accumulate combiner initial sequence)
  (if (null? sequence)
      initial
      (combiner (car sequence) (accumulate combiner initial
					   (cdr sequence)))))

(define (enumurate-tree tree)
  (cond ((null? tree) nil)
	((list? tree)
	 (append (enumerate-tree (car tree)) (enumerate (cdr tree))))
	(else (list tree))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (acc-map func sequence)
  (accumulate (lambda (x y) (cons (func x) y))
	      nil sequence))




