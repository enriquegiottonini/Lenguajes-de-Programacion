#lang plait

(require "arithlang.rkt")
(print-only-errors #t)

;; Tests para eval method

;; happy path
;; numeros
(test (eval `5) 5)
(test (eval `-1) -1)
(test (eval `3.14) 3.14)

;; sumas
(test (eval `{+ 1 2}) 3)
(test (eval `{+ -1 -1}) -2)
(test (eval `{+ 1.5 1.5}) 3)

;; multiplicacion
(test (eval `{* 2 2}) 4)
(test (eval `{* 0 2}) 0)
(test (eval `{* -2.5 -2}) 5)

;; resta binaria
(test (eval `{- 2 2}) 0)
(test (eval `{- 2.5 -2}) 4.5)

;; negacion unitaria
(test (eval `{- 5}) -5)
(test (eval `{- -5}) 5)

;; nested expressions
(test (eval `{+ (* 2 2) 1}) 5)
(test (eval `{+ (* 1 1) (+ (* 2 2) (* 3 3))}) 14)
(test (eval `(- (- (- (- (- (- (- (- (- 1)))))))))) -1)

;; malformadas
(test/exn (eval `{}) "parse: expresion malformada.")
(test/exn (eval `{1 + 2}) "parse: expresion malformada.")
(test/exn (eval `{+ 1}) "parse: expresion malformada.")
(test/exn (eval `{+ 1 2 3}) "parse: expresion malformada.")
(test/exn (eval `{/ 5 5}) "parse: expresion malformada.")
(test/exn (eval `{+ 1 (+ 2 (+ 3))}) "parse: expresion malformada.")
(test/exn (eval `{- 1 (-(- 2 2 2) 3)}) "parse: expresion malformada.")

(display "no output means no errors or lack of better unit testing. :)\n")