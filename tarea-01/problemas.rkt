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

;; 14
;; div	: (int, int) -> int
(define div
  (lambda (dividend divisor)
    (cond
      [(< dividend 0)(error "No lo divide completamente")]
      [(zero? dividend) 0]
      [else  (+ 1 (div (- dividend divisor) divisor))])))

;; 15
;; append-map	: (procedure, list) -> list
;;		   procedure : 
(define append-map
  (lambda (proc lst)
    (if (empty? lst)
	lst
	(append (proc (car lst))
	        (append-map proc (rest lst))))))

;; 16
;; set-difference	: (list, list) -> list
(define set-difference
  (lambda (lst1 lst2)
    (if (empty? lst1)
	'()
	(if (in-L? (car lst1) lst2)
	    (set-difference (rest lst1) lst2)
	    (cons (car lst1) (set-difference (rest lst1) lst2))))))
;; set-difference:helper
(define (in-L? elmt lst)
    (if (empty? lst)
	#f
	(or (eqv? elmt (car lst))
	    (in-L? elmt (rest lst)))))

;; 17
;; foldr	: (operator ? list) -> ?
(define foldr
  (lambda (op acc lst)
    (if (empty? lst)
	acc
	(op (car lst) (foldr op acc (rest lst))))))

;; 18
;; powerset	: list -> list
(define powerset
  (lambda (lst)
    (if (empty? lst)
	(cons '() '())
	(append (add-head (car lst) (powerset (rest lst)))
	      (powerset (rest lst))))))

;; add-head	: (symbol, list) -> list
;; usage (add-head elmt lst) = given a list lst of lists, add elmt as a head to each element of lst. 
(define add-head
  (lambda (elmt lst)
    (if (empty? lst)
	'()
	(cons (cons elmt (car lst))
	      (add-head elmt (cdr lst))))))

;; 19
;; cartesian-product	: list -> list
;; solo pude hacerlo cuando n=2.
(define cartesian-product
  (lambda (lst)
    (foldr cross '(()) lst)))

;; A x B = {(a, b) : a in A, b in B}
(define (cross A B)
  (if (or (empty? A)
	  (empty? B))
	 '() 
	  (append (map (lambda (elmt) 
			 (cons (car A) elmt))
		       B)
		  (cross (cdr A) B))))

;; 20
;; 20.1
(define (insertL-fr x y lst)
  (define (inserts-if head acc)
    (if (eq? x head)
	(cons y (cons head acc))
	(cons head acc)))
  (foldr inserts-if '() lst))

;; 20.2
(define (filter-fr predicate lst)
  (define (filter head acc)
    (if (predicate head)
	(cons head acc)
	acc))
  (foldr filter '() lst))

;; 20.3
(define (map-fr proc lst)
  (define (transform head acc)
    (cons (proc head) acc))
  (foldr transform '() lst))

;; 20.4
(define (append-fr lst1 lst2)
  (define (unite head acc)
	(cons head acc))
  (foldr unite lst2 lst1))

;; 20.5
(define (reverse-fr lst)
  (define (add head acc)
	(append acc (list head)))
  (foldr add '() lst))

;; 20.6
(define (binary->natural-fr lst)
  (define (binary-sum head acc)
    (if (empty? head)
	head
	(+ head (* 2 acc))))
  (foldr binary-sum 0 lst))

;; 20.7
(define (append-map-fr proc lst)
  (define (append-map head acc)
    (append (proc head) acc))
  (foldr append-map '() lst))

;; 20.8
(define (set-difference-fr lst1 lst2)
  (define (rmv head acc)
    (if (member head lst2)
	acc
	(cons head acc)))
  (foldr rmv '() lst1))

;; 20.9
(define (powerset-fr lst)
  (define (powerset head acc)
    (append (map (lambda (ls) (cons head ls)) acc)
	    acc))
  (foldr powerset '(()) lst))

;; 21
(define snowball
  (letrec
    ((odd-case
       (lambda (fix-odd)
	 (lambda (x)
	   (cond
	     ((and (exact-integer? x) (positive? x) (odd? x))
	      (snowball (add1 (* x 3))))
	     (else (fix-odd x))))))
     (even-case
       (lambda (fix-even)
	 (lambda (x)
	   (cond
	     ((and (exact-integer? x) (positive? x) (even? x))
	      (snowball (/ x 2)))
	     (else (fix-even x))))))
     (one-case
       (lambda (fix-one)
	 (lambda (x)
	   (cond
	     ((zero? (sub1 x)) 1)
	     (else (fix-one x))))))
     (base
       (lambda (x)
	 (error 'error "Invalid value ~s~n" x))))
  (one-case (even-case (odd-case base)))))

(define quine "no lo hice.")

(provide (all-defined-out))
