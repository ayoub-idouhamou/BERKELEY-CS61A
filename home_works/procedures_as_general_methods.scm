#lang scheme

;;Fixed-point as a general method of computation
(define (fixed-point func first-guess)
  (define tolerance .00000000000001)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (iterate guess)
    (let ((next (func guess)))
      (display next)
      (newline)
      (cond ((good-enough? next guess) next)
            (else (iterate next)))))
  (iterate first-guess))

(define square-root
  (lambda (x) (fixed-point (lambda (y) (/ (+ (/ x y) y) 2.0))
                           1.0)))

;;Exercise 1.35
;;We can easily show that the golden ratio fi is a fixed point for the function  x --> 1 + 1 / x
;;by substituting (1 + sqrt(5)) / 2.0 for the value of fi in the equation
;; x = 1 + 1 / x and we find that it is a solution for it.
;; Using this fact we can readily approximate the golden ration using the fixed
;;point procedure
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;;Exercise 1.36
;;Finding a solution for x^x = 1000 is equivalent of finding
;;a fixed point for the function x --> log(1000) / log(x)
(define solution
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2.0))

;;Exercise 1.37: Infinite continued fraction
;;recursive version
(define recur-continued-fraction
  (lambda (num denom k)
    (define recur
      (lambda (i)
        (if (> i k)
            0
            (/ (num i) (+ (denom i) (recur (+ i 1)))))))
    (recur 1)))
;;iterative version
(define iter-continued-fraction
  (lambda (num denom k)
    (define iterate (lambda (i accum)
      (cond ((= i 0) accum)
            (else (iterate (- i 1)
                           (/ (num i) (+ (denom i) accum)))))))
    (iterate k 0)))

;; In order to compute a fixed point of a function more quickly (in fewer steps),
;; we make a transformation to the function by averaging succissive approximations,
;; a technique called average damping, that eliminate oscillations about the solution
;; that may occur

;;Exercise 1.38: An approximation of e (the base of natural logarithms)
;;using a continued fraction expansion of e - 1 discovered by the mathematician Euler
;;I admit I didn't come up with the procedure representing the pattern of sequence values of Di
;; in the expansion, I am not that impressive in mathematics
(define approximate-e
  (lambda (k)
    (+ (iter-continued-fraction
          (lambda (i) 1.0)
          (lambda (i) (if (= (remainder i 3) 2) (/ (+ i 1) 1.5) 1))
          k)
       2)))
                                      
;; Exercise 1.39: A procedure representing an approximation to the tangent function
;; of some given angle x, using k-term continued fraction (Lambert's expansion formula)
(define (cont-frac-tan x k)
  (/ (iter-continued-fraction (lambda (i) (- (* x x)))
                              (lambda (i) (- (* 2 i) 1))
                              k)
     (- x)))


   
 
                                                          