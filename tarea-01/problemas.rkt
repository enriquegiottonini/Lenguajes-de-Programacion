#lang racket

;; 1
;; countdown	: natural -> list
;; usage	: (countdown n) = (n n-1 ... 0)
(define countdown
  (lambda (n)
    (if (zero? n)
	'(0)
	(cons n (countdown (- n 1))))))

;; 2
;; insertL	: (symbol , symbol , list) -> list
;; usage	: (insertL wanted new lst) = list with inserted elements 'new before each instance of 'wanted in lst.
(define insertL
  (lambda (wanted new lst)
    (if (empty? lst)
	lst
	(if (eqv? wanted (car lst))
	    (cons new (cons (car lst) (insertL wanted new (rest lst))))
	    (cons (car lst) (insertL wanted new (rest lst)))))))

;; 3
;; remv-1st	: (symbol, list) -> list
;; usage	: (remv-1st sym lst) = list without the first instance of sym in lst.
(define remv-1st
  (lambda (sym lst)
    (if (empty? lst)
	lst
	(if (eqv? sym (car lst))
	    (rest lst)
	    (cons (car lst) (remv-1st sym (rest lst)))))))

;; 4 
;; map		: (procedure, list) -> list
(define map
  (lambda (proc lst)
    (if (empty? lst)
	lst
	(cons (proc (car lst)) (map proc (cdr lst))))))

;; 5
;; filter	: (procedure, list) -> list
(define filter
  (lambda (proc lst)
    (if (empty? lst)
	lst
	(if (proc (car lst))
	    (cons (car lst) (filter proc (cdr lst)))
	    (filter proc (cdr lst))))))

;; 6
;; zip		: (list, list) -> list
(define zip
  (lambda (lst1 lst2)
    (cond [(empty? lst1) '()]
	  [(empty? lst2) '()]
	  [else (cons (cons (car lst1)
			    (car lst2))
		      (zip (cdr lst1)
			   (cdr lst2)))])))

;; 7
;; list-index-ofv : (symbol, list) -> int
(define list-index-ofv
  (lambda (sym lst)
    (if (empty? lst)
	(error "index out of bounds.")
	(if (eqv? sym (car lst))
	    0
	    (+ 1 (list-index-ofv sym (cdr lst)))))))

;; 8
;; append	: (list, list) -> list
(define append
  (lambda (lst1 lst2)
    (if (empty? lst1)
	lst2
	(cons (car lst1) (append (cdr lst1) lst2)))))

;; 9
;; reverse	: list -> list
(define reverse
  (lambda (lst)
    (if (empty? lst)
	lst
	(append (reverse (cdr lst)) (cons (car lst) '())))))

; 10
;; repeat	: (list, int) -> list
(define repeat
  (lambda (lst ditto)
    (if (zero? ditto)
	'()
	(append lst (repeat lst (- ditto 1))))))

;; 11
;; same-lists*	: (list, list) -> Boolean
(define same-lists*
  (lambda (lst1 lst2)
      (if (and (empty? lst1) (empty? lst2))
	  #t
	  (if (or (empty? lst1) (empty? lst2))
	      #f
	      (let* ([head1 (car lst1)]
		     [head2 (car lst2)])
		(cond 
		  [(or (and (pair? head1) (not (pair? head2))); head type disparity case
		   (and (not (pair? head1)) (pair? head2))) #f]
		  [(pair? head1) (and (same-lists* head1 head2)
				  (same-lists* (rest lst1) (rest lst2)))]
		  [else (and (eqv? head1 head2)
			 (same-lists* (rest lst1) (rest lst2)))]))))))
#|
   12 
   ((w x) (z))
 = ((w (x . ())) (z . ()))
 = ((w . (x . ())) . (z . ()))
|#

;; 13
;; binary->natural : list -> int
(define (binary->natural lst)
  (define (binary->natural:helper lst count)
    (if (empty? lst)
	0
	(+ (* (car lst) (expt 2 count))
	   (binary->natural:helper (cdr lst) (+ count 1)))))
  (binary->natural:helper lst 0))


(provide (all-defined-out))
