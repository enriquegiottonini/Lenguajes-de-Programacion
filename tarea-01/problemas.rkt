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

(provide (all-defined-out))
