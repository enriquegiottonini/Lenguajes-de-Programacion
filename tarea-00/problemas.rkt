#lang racket

;; 1.
(define pi 3.14)

;; 2.
(define (area-circle r)
  (if (< r 0)
      -1
      (* pi (* r r))))

;; 3.
(define (circle-properties r)
  (define (perimeter-circle r)
    (if (< r 0)
	-1
	(* 2 (* pi r))))
  (list (area-circle r)
	(perimeter-circle r)))

;; 4.
(define (rectangle-properties rec)
 (let* ([largo (list-ref rec 0)]
	[ancho (list-ref rec 1)])
   (if (or (< largo 0) (< ancho 0))
       -1
       (list (* largo ancho)
	 (* 2 (+ largo ancho))))))

;; 5.
(define (find-needle ls)
  (define (find-needle-iter ls counter)
    (cond [(empty? ls) -1]
	  [(equal? (first ls) 'needle) counter]
	  [else (find-needle-iter (rest ls) (+ counter 1))]))
  (find-needle-iter ls 0))

;; 6.
(define (abs x)
  null)

;; 7.
(define (inclis1 ls)
  null)

;; 8.
(define (even? x)
  null)

;; 9.
(define another-add
  (lambda (n m)
    null))

(provide (all-defined-out))
