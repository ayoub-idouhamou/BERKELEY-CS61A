
;; reverses a list
(define (reverse a-list)
    (cond ((null? a-list) a-list)
	  (else (append (reverse (cdr a-list))
			(list (car a-list))))))
;; an iterative version of reverse
(define (iter-reverse a-list)
  (define (iterate old new)
    (if (null? old)
        new
        (iterate (cdr old) (cons (car old) new))))
  (iterate a-list '()))

;; counts the leaves of a tree
(define (count-leaves a-tree)
  (cond ((null? a-tree) 0)
        ((not (pair? a-tree)) 1)
        (else (+ (count-leaves (car a-tree))
                 (count-leaves (cdr a-tree))))))
                             
;; EX 2.27
;; deep reverses a tree
(define deep-reverse
  (lambda (tree)
    (if (not (pair? tree))
        tree
        (reverse (map deep-reverse tree)))))

;; EX 2.28
;; Enumerates the leaves of a tree into a list
(define (fringe items)
  (if (not (list? items))
      (list items)
      (reduce append (map fringe items))))

;; EX 2.29 Binary mobile
(define (make-mobile left right) (list left right))
(define left-branch car)
(define right-branch cadr)
(define mobile? list?)
(define weight? number?) 

(define (make-branch length structure) (list length structure))
(define branch-length car)
(define branch-structure cadr)

			  
(define (total-weight mobile)
  (define (weight branch)
    (let ((structure (branch-structure branch)))
      (if (mobile? structure)
	  (total-weight structure)
	  structure)))  
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define (torque branch)
  (let ((structure (branch-structure branch)))
    (* (branch-length branch)
       (if (mobile? structure)
	   (total-weight structure)
	   structure))))

(define (balanced? mobile)
  (define (balance structure)
    (if (not (mobile? branch))
	true
	(balanced? structure)))
  (let ((left (left-branch mobile)) (right (right-branch mobile)))
    (and (= (torque left) (torque right))
	 (balance (branch-structure left))
	 (balance (branch-structure right)))))

;; In the situation, where we need to change the representation of our abstract
;;data types, namely mobile and branch, we only need to adapt our selectors
;; to the new implementations; in the other hand we can keep our framework of
;; opertions (total-weight, torque, balanced..) untouched, due to the fact that
;; we respected the data abstraction principle.

;; Some test samples
(define b1 (make-branch 2 9))
(define b2 (make-branch 6 3))
(define m1 (make-mobile b1 b2))

(define b3 (make-branch 2 10))
(define b4 (make-branch 4 5))
(define m2 (make-mobile b3 b4))
(define m3 (make-mobile (make-branch 7 m1) (make-branch 8 m2)))


(define t1 (= (total-weight m1) 12))
(define t2 (= (total-weight m2) 15))
(define t3 (= (total-weight m3) 27))
(define t4 (= (torque (left-branch m3)) 84))
(define t5 (= (torque (right-branch m3)) 120))

