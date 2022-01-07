#lang scheme
(require berkeley)

(define (print-line something)
  (display something) (newline))

(define (func n)
  (if (< n 3)
      n
      (+ (func (- n 1))
         (* 2 (func (- n 2)))
         (* 3 (func (- n 3))))))

(define (iterate-func n)
  (define (iterate a b c counter)
    (if (< counter 3)
        c
        (iterate b c (+ c (* 2 b) (* 3 a))
                 (- counter 1))))
  (if (< n 3)
      n
      (iterate 0 1 2 n)))

(define (expt b n)
  (define (iterate a b n)
    (cond ((or (= n 0) (= n 1)) a)
          ((even? n) (iterate (square b) (square b) (/ n 2))) 
          (else (* b (iterate a b (- n 1))))))
  (iterate 1 b n))

(define (expt1 b n)
	(cond ((= n 0) 1)
		((even? n) (square (expt b (/ n 2))))
		(else (* b (expt  b (- n 1))))))
		
(define (square x) (* x x))

(define (pascal-elem column row)
  (cond ((= column 0) 1)
        ((= column row) 1)
        (else (+ (pascal-elem (- column 1) (- row 1))
                 (pascal-elem column (- row 1))))))

(define (cont-frac-rec numer denom k)
  (define (iterate n)
    (if (> n k)
        0
        (/ (numer n) (+ (denom n)
                        (iterate (+ n 1))))))
  (iterate 1))

(define (cont-frac-iter n d k)
  (define (iterate result i)
    (if (= i 0)
        result
        (iterate (/ (n i) (+ (d i) result)) (- i 1))))
  (iterate 0.0 k))

(define (f i)
  (define (iter a b c n)
    (if (= n i)
        a
        (iter b c (if (= a 1) 1 (+ a 2)) (+ n 1))))
  (iter 1 2 1 1))
    
(define (euler-frac k)
  (cont-frac-rec (lambda (i) 1.0) f k))


(define (tan-cf x k)
  (if (= x 0)
      0
      (/ (cont-frac-iter (lambda (i) (- (* x x)))
                (lambda (i) (- (* 2 i) 1)) k) x)))

(define (next-perf n)
  (define (sum-factors i j result)
    (cond ((= i j) result)
          ((= (remainder j i) 0) (sum-factors (+ i 1)
                                              j (+ result i)))
          (else (sum-factors (+ i 1) j result))))
  (define (next j)
    (if (= (sum-factors 1 j 0) j)
        j
        (next (+ j 1))))
  (next n))

(define (number-of-partitions integer)
  (define (partitions pieces number)
    (cond ((= number 0) 1)
          ((or (empty? pieces) (< number 0)) 0)
          (else (+ (partitions pieces (- number (first pieces)))
                   (partitions (bf pieces) number)))))
  (define digits '(9 8 7 6 5 4 3  2 1))
  (partitions digits integer))
           








  


