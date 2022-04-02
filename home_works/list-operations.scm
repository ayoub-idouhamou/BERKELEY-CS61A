(define (list-refs items n)
  (if (= n 0)
      (car items)
      (list-refs (cdr items) (- n 1))))
      
(define (length items)
  (define (count-items items count)
    (if (null? items)
        count
        (count-items (cdr items) (+ count 1))))
  (count-items items 0))

(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))))

;; EX 2.17		
(define (last-pair a-list)
  (cond ((null? a-list)
      (error "last-pair: contract violation
         expected: a list
         given: '()"))
      (else (define (find-last items)
              (if (= (length items) 1)
                (car items)
                (find-last (cdr items))))
            (find-last a-list))))

;; EX 2.18
(define (reverse a-list)
	(if (null? a-list)
		a-list
		(append (reverse (cdr a-list)) (list (car a-list)))))
		
;; EX 2.19
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define (count-change amount coin-values)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+ (count-change amount (except-first-denomination coin-values))
         (count-change (- amount (first-denomination coin-values))
             coin-values)))))
             
;; EX 2.20		
(define (same-parity x . xs)
  (cons x (filter (if (even? x) even? odd?) xs)))


	
	
	
	
	
