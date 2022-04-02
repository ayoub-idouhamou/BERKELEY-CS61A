;; EX 2.21
(define square (lambda (x) (* x x)))
(define (square-list items)
  (if (null? items)
    items
    (cons (square (car items)) (square-list (cdr items)))))	

(define square-list (lambda (items) (map square items)))

;; EX 2.23
(define (for-each proc a-list)
  (cond ((null? a-list) true)
        (else (proc (car a-list))
              (for-each proc (cdr a-list)))))
        
