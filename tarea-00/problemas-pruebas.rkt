#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "area-circle"
    (check-eqv? (area-circle 5) 78.5)
    (check-eqv? (area-circle 5.5) 94.985)
    (check-eqv? (area-circle 11/2) 94.985)
    (check-eqv? (area-circle 0) 0)
    (check-eqv? (area-circle -3) -1))
  
  (test-case "circle-properties"
    (check-within (circle-properties 5)   '(78.5 31.4) 0.001)
    (check-equal? (circle-properties 5.5) '(94.985 34.54))
    (check-equal? (circle-properties 11/2) '(94.985 34.54))
    (check-equal? (circle-properties 0)   '(0 0))
    (check-equal? (circle-properties -1)   '(-1 -1)))

  (test-case "rectangle-properties"
    (check-equal? (rectangle-properties '(2 4)) '(8 12) 0.001)
    (check-equal? (rectangle-properties '(2.5 4.5)) '(11.25 14.0))
    (check-equal? (rectangle-properties '(5/2 9/2)) '(45/4 14))
    (check-equal?  (rectangle-properties '(0 0)) '(0 0))
    (check-eqv? (rectangle-properties '(2 -4)) -1)
    (check-eqv? (rectangle-properties '(-2 4)) -1))
  
  (test-case "find-needle"
    (check-eqv? (find-needle '(hay needle hay)) 1)
    (check-eqv? (find-needle '(hay hay hay)) -1)
    (check-eqv? (find-needle '()) -1)
    (check-eqv? (find-needle '(needle needle hay)) 0))
  
  (test-case "abs"
    (check-eqv? (abs 3) 3)
    (check-eqv? (abs -2) 2))
  
  (test-case "inclis1"
    (check-equal? (inclis1 '(1 2 3)) '(2 3 4))
    (check-equal? (inclis1 '()) '()))
  
  (test-case "even?"
    (check-equal? (map even? '(1 2 3 4 5 6)) '(#f #t #f #t #f #t))
    (check-equal? (even? -5) #f)
    (check-equal? (even? 0) #t))
  
  (test-case "another-add"
    (check-eqv? (another-add 10 5) 15)))

(run-tests pruebas 'verbose)
