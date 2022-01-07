#lang racket

(define (make-segment start end)
  (cons start end))
(define start-segment car)
(define end-segment   cdr)

(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)

(define (mid-point segment)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((start (start-segment segment))
        (end   (end-segment   segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rectangle up-segment right-segment)
  (cons up-segment right-segment))

(define (height rectangle)
  (abs (- (x-point (start-segment (car rectangle)))
          (x-point (end-segment (car rectangle))))))

(define (width rectangle)
  (abs (- (y-point (start-segment (cdr rectangle)))
          (y-point (end-segment   (cdr rectangle))))))

(define (perimeter rectangle)
  (+ (* 2 (height rectangle))
     (* 2 (width  rectangle))))

(define (cons1 x y)
  (lambda (m) (m x y)))
(define (car1 z)
  (z (lambda (p q) p)))

  