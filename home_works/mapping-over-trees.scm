;; EX 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define square (lambda (x) (* x x)))

;; square-tree that uses map and recursion
(define (square-tree tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree subtree) (square subtree)))
       tree))

;; EX 2.31
;; Abstracting over the thing to do to every leaf
;; to a function argument
(define (tree-map func tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map func subtree) (func subtree)))
       tree))

;; redefining square-tree
(define square-tree
  (lambda (tree) (tree-map square tree)))

;; EX 2.32
(define (subsets set)
  (if (null? set)
      (list nil)
      (let ((rest (subsets (cdr set))))
	(append rest
		(map (lambda (subset) (cons (car set) subset))
		     rest)))))
