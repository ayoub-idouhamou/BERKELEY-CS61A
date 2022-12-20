#lang scheme

(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))

(define (exp-fast b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp-fast b (/ n 2))))
        (else (* b (exp-fast b (- n 1))))))



(define square (lambda (x) (* x x)))

(define (multiply a b)
  (if (= b 0) 0 (+ a (multiply a (- b 1)))))

(define (iter-multiply a b)
  (define (iter counter result)
    (if (= counter 0)
        result
        (iter (- counter 1) (+ a result))))
  (iter b 0))

(define (fast-multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-multiply a (halve b))))
        (else (+ a (fast-multiply a (- b 1))))))

(define (fast-iter-multiply a b)
  (define (iter counter result)
    (cond ((= counter 0) result)
          ((even? counter) (iter (halve counter)
                                 (double result)))
          (else (iter (- counter 1) (+ result a)))))
  (iter b a))

(define double (lambda (n) (* n 2)))
(define halve (lambda (n) (/ n 2)))

