#lang scheme

;;Ex 1.3 sum of squares of two large numbers out of three
(define square (lambda (x) (* x x)))
(define sum-squares
  (lambda (a b) (+ (square a) (square b)))) 
(define sum-sq-large-two-nums
  (lambda (x y z)
    (cond ((and (>= x y) (>= y z))(sum-squares x y))
          ((and (>= x y) (<= y z))(sum-squares x z))
          ((and (<= x y) (>= x z)) (sum-squares x y))
          (else (sum-squares y z)))))

;;Ex 1.4
(define a-plus-abs-b
  (lambda (a b) ((if (> b 0) + -) a b)))
;; As we know, the language allows us to write combinations
;;whose operators are themselves compound expressions.
;;In our case the operator is expressed as an if expression.
;;we apply an addition operation if b is positive, soustraction otherwise.
;;in other words we are making addition with b replaced by absolute value.

;;Ex 1.5 Applicative versus normal evaluation
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))
;; We gonna try to contrast the behavior of evaluating this
;;expression (test 0 (p)) for the two modes of evaluation
;;in the applicative mode, the interpreter will start to
;;evaluate the operator and operands of the combination before
;;applying the operator. But when it evalutes (p), it causes
;;the interpreter to enter infinite loop (due to looping implementation
;;of p), so the process never terminates.
;; In the other hand, using normal evaluation, avoids us to enter this infinite loop,
;;because the second operand is never evaluated, even in the body of test (x == 0)
;;(test 0 (p)) in this case returns 0

;; Ex 1.6
;; In the new-if implementation, the two clauses given as arguments are always evaluated before applying new-if.
;;Without even looking at the result of the predicate, and this is not how case analysis works.
;;An "if" expression works like this: we evaluate only one clause based on the predicate.
;;If we use this "new-if" procedure, we have got a non-terminating loop; To go around this problem, we could instruct
;;the compiler to the normal evaluation strategy.

;;Ex 1.10: Ackermann's function
(define (acker x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (acker (- x 1) (acker x (- y 1))))))

(define f (lambda (n) (acker 0 n)))
(define g (lambda (n) (acker 1 n)))
(define h (lambda (n) (acker 2 n)))
(define k (lambda (n) (* 5 n n)))
;;f(n) computes 2*n
;;g(n) computes 2**n
;;k(n) computes 5*n*n

;;Ex 1.11
;;recursive process
(define (func-recur n)
  (cond ((< n 3) n)
        (else (+ (func-recur (- n 1))
                 (* 2 (func-recur (- n 2)))
                 (* 3 (func-recur (- n 3)))))))
;;An iterative version using four state variables
(define (func-iter n)
  (define (iter a b c m)
    (cond ((= m n) c)
          (else (iter b c (+ c (* 2 b) (* 3 a)) (+ m 1)))))
  (if (< n 3) n (iter 0 1 2 2)))

;;Ex 1.12 Pascal's Triangle
(define (pascal column row)
  (if (or (= column 0) (= column row))
      1
      (+ (pascal (- column 1) (- row 1))
         (pascal column (- row 1))))) 
                



