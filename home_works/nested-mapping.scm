(load "sequences-as-conventional-interfaces")

;; An implemetation for testing primality
(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= (smallest-divisor n) n))

;; Given an input integer n , this procedure generates ordered pairs (j, i)
;; such that j + i is prime, where 1 <= j < i <= n 
(define (prime-sum-pairs n)
  (map make-triple (filter prime-sum? (unique-pairs n))))

(define pair-sum (lambda (pair) (+ (car pair) (cadr pair))))

(define make-triple (lambda (pair) (append pair (list (pair-sum pair)))))

(define prime-sum? (lambda  (pair) (prime? (pair-sum pair))))

;; A convenient procedure combining mapping and appending together
(define flat-map
  (lambda (proc seq) (fold-right append nil (map proc seq))))
  
(define (permutations set)
  (if (null? set)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x set))))
	       set)))

;; EX 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
	 (map (lambda (j) (list i j))
	      (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))

;; EX 2.41
(define (unique-triples-sum=s n s)
  (filter (lambda (triple) (= (sum-triple triple) s))
	  (flatmap (lambda (i) (map (lambda (pair) (cons i pair))
				    (unique-pairs (- i 1))))
		   (enumerate-interval 1 n))))

(define sum-triple (lambda (triple) (accumulate + 0 triple)))
