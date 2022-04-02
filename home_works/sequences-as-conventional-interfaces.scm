;; SEQUENCE OPERATIONS

;; An implemetation for accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; EX 2.33
;; expressing some sequence operations in terms of accumulate
(define (map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y))
	      nil
	      sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; EX 2.34
;; Emplementing Horner's Rule for evaluating polynomials as an accumulation
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;; EX 2.35
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (subtree)
			 (if (pair? subtree) (count-leaves subtree) 1))
		       tree)))

;; EX 2.36
(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op initial (map car seqs))
	    (accumulate-n op initial (map cdr seqs)))))

;; EX  2.37
;; An attempt to implement operations that manipulate vectors and matrices
(define (dot-product u v)
  (accumulate + 0 (accumulate-n * 1 (list u v))))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row))
	 m)))

;; EX 2.38
(define (fold-left op initial sequence)
  (define (iter-fold result rest)
    (if (null? rest)
	result
	(iter-fold (op result (car rest)) (cdr rest))))
  (iter-fold initial sequence))

(define fold-right accumulate)

;; EX 2.39 expressing reverse in terms of folding
(define reverse
  (lambda (sequence)
    (fold-right (lambda (x result) (append result (list x)))
		nil
		sequence)))

(define reverse1
  (lambda (sequence)
    (fold-left (lambda (result x) (cons x result))
	       nil
	       sequence)))
