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
;; usage	: (insertL wanted ins lst) = list with inserted elements 'new before each instance of 'wanted in lst.
(define insertL
  (lambda (wanted new lst)
    (if (empty? lst)
	lst
	(if (eqv? wanted (car lst))
	    (cons new (cons (car lst) (insertL wanted new (rest lst))))
	    (cons (car lst) (insertL wanted new (rest lst)))))))
(provide (all-defined-out))
