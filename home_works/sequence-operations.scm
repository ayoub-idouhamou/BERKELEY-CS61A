
(define (simple-map func items)
  (if (null? items)
      nil
      (cons (func (car items)) (map func (cdr items)))))

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
(define (map-tree func tree)
  (cond ((null? tree) nil)
	((list? tree)
	 (cons (map-tree func (car tree)) (map-tree func (cdr tree))))
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
;; Exercise 2.33
(define (acc-map func sequence)
  (accumulate (lambda (x y) (cons (func x) y))
	      nil sequence))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;Exercise 2.34: Evaluating a polynomial at a given point x
;;using the well-known algorithm: Horner's Rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0 coefficient-sequence))

;;Exercise 2.35: a sequence operation that counts tree leaves
;; using accumulation and enumeration
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (leaf) 1) (enumurate-tree tree))))

;;Exercise 2.36
;;(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9))) --> (12 15 18)
(define (accumulate-n combiner initial sequences)
  (if (null? (car sequences))
      nil
      (cons (accumulate combiner initial (map car sequences))
	    (accumulate-n combiner initial (map cdr sequences)))))

;;Execise 2.37: basic matrix and vector operations
(define (dot-product u v)
  (accumulate + 0 (map * u v)))

(define (matrix-*-vector matrice vector)
  (map (lambda (row) (dot-product vector row)) matrice))

(define (transpose matrice)
  (accumulate-n cons nil matrice))

(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector n-cols m-row))
	 m)))
