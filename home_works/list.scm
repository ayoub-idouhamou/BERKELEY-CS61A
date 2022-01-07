         
(define iter-deep-reverse
  (lambda (a-list)
    (define iterate
      (lambda (items reversed)
	(if (null? items)
	    reversed
	    (let ((head (car items))
		  (tail (cdr items)))
	      (iterate tail (if (pair? head)
				(cons (deep-reverse head) reversed)
				(cons head reversed)))))))
    (iterate a-list nil)))

(define recur-deep-reverse
  (lambda (a-list)
    (cond ((<= (length a-list) 1) a-list)
	  (else
	   (let ((head (car a-list))
		 (tail (cdr a-list)))
	     (append (recur-deep-reverse tail)
		     (if (pair? head)
			 (list (recur-deep-reverse head))
			 (list head))))))))
	      
(define fringe
  (lambda (a-list)
    (if (null? a-list)
	a-list
	(let ((head (car a-list))
	      (tail (cdr a-list)))
	  (if (pair? head)
	      (append (fringe head) (fringe tail))
	      (cons head (fringe tail)))))))
	      

(define reverse
  (lambda (a-list)
    (cond ((null? a-list) nil)
	  ((equal? (length a-list) 1) a-list)
	  (else (append (reverse (cdr a-list))
			(list (car a-list)))))))

(define iter-reverse
  (lambda (a-list)
    (define iterate
      (lambda (items result)
	(if (null? items)
	    result
	    (iterate (cdr items) (cons (car items) result)))))
    (iterate a-list nil)))
