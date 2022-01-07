#lang racket

(require scheme)

(define (make-interval x y)
  (cons x y))
(define lower-bound car)
(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define substract-interval
  (lambda (m n) 
    (add-interval m (make-interval (- (lower-bound n))
                                   (- (upper-bound n))))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define width
  (lambda (interval) (/ (- (upper-bound interval)
                           (lower-bound interval)) 2)))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error, interval spans 0" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
(define (make-center-percent center percent)
  (let ((tolerance (* center (/ percent 100))))
    (make-interval (- center tolerance)
                 (+ center tolerance))))
(define (center interval)
  (/ (+ (lower-bound interval)
        (upper-bound interval))
     2))
(define (percent interval)
  (* 100 (- 1 (/ (lower-bound interval) (center interval)))))

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

(define (last-pair a-list)
  (cond ((null? a-list)
      (error "last-pair: contract violation
         expected: a pair
         given: '()"))
      (else (define (find-last items)
              (if (= (length items) 1)
                (car items)
                (find-last (cdr items))))
            (find-last a-list))))

(define (for-each proc a-list)
  (cond ((null? a-list) 'done)
        (else (proc (car a-list))
              (for-each proc (cdr a-list)))))

(define (substitute word-list old-word new-word)
  (define (replace word)
    (if (equal? word old-word)
        new-word
        word))
  (define (recur-substitute words)
    (if (empty? words)
        '()
        (cons
         (if (list? (car words))
             (recur-substitute (car words))
             (replace (car words)))
         (recur-substitute (cdr words)))))
  (recur-substitute word-list))

(define (substitute2 word-list old-words new-words)
  (define (replace word old-words new-words)
    (cond ((empty? old-words) word)
          ((equal? word (car old-words)) (car new-words))
          (else (replace word (cdr old-words) (cdr new-words)))))
  (define (recur-substitute words)
    (if (empty? words)
        '()
        (cons
         (if (list? (car words))
             (recur-substitute (car words))
             (replace (car words) old-words new-words))
         (recur-substitute (cdr words)))))
  (recur-substitute word-list))
  

(define (sort sent)
  (if (empty? sent)
      '()
      (insert (car sent)
              (sort (cdr sent)))))

(define (insert num sent)
  (cond ((empty? sent) '())
        ((< num (car sent)) (list num sent))
        (else (list (car sent) (insert num (cdr sent))))))
