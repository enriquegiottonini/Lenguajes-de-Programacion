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
 (list (area-circle r)
       (* 2 (* pi r))))

;; 4.
(define (rectangle-properties rec)
 (let* ([largo (list-ref rec 0)]
	[ancho (list-ref rec 1)])
   (list (* largo ancho)
	 (* 2 (+ largo ancho)))))

;; 5.
(define (find-needle ls)
  null)

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
