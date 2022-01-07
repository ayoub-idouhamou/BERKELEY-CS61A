

;; A procedure that counts syllables in a word,
;; by only counting the vowels 
;; (consecutive ones counts for one)
(define syllables 
  (lambda (a-word)
    (define vowel?
      (lambda (char) (member char '(a e i o u))))
    (define count
      (lambda (a-word consecutive acc)
        (cond ((empty? a-word) acc)
              ((and (vowel? (first a-word)) (not consecutive))
               (count (butfirst a-word) true (+ acc 1)))
              (else (count (butfirst a-word) false acc)))))
    (count a-word false 0)))


;; takes a predicate ordering and a list as input
;; and checks if the list is ordered accoridng to ordering
(define in-order?
  (lambda (ordering a-list)
    (cond ((< (length a-list) 2) true)
          ((ordering (first a-list) (first (rest a-list)))
            (in-order? ordering (rest a-list)))
          (else false))))
          
;;predicate: a function that defines ordering
;;returns a function with one argument, the list to check order of)
;;according to predicate
(define order-checker
  (lambda (predicate)
    (define iterate-check
      (lambda (a-list)
        (cond ((< (length a-list) 2) true)
              ((predicate (first a-list) (first (rest a-list)))
               (iterate-check (rest a-list)))
              (else false))))
    iterate-check))
    
;;constructor and selctor procedures for the abstract data type time
(define (make-time hr mn cat) (list hr mn cat))
(define hour car)
(define minute cadr)
(define category caddr)

(define (time-print-format time)
  (word (hour time) : (minute time) (category time)))

(define (24-hour time)
  (let ((category (category time))
	(minute (minute time))
	(hour (hour time)))
    (if (equal? category 'am)
	(+ (* 100 hour) minute)
	(+ (* 100 (+ 12 hour)) minute))))


















