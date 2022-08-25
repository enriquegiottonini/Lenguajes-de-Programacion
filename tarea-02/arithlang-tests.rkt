#lang plait

(require "arithlang.rkt")

;; Tests para eval
;; happy path
;; simple expressions
(test (eval `5) 5)
(test (eval `{+ 1 2}) 3)
(test (eval `{* 2 2}) 4)
(test (eval `{- 2 2}) 0)
(test (eval `{- 5}) -5)
;; nested expressions
(test (eval `{-,`{- 1}}) 1)
(test (eval `{+,`{* 2 2} 1}) 5)
;; unhappy path
(test/exn (eval `{}) "parse: expresion aritmetica malformada.")
(test/exn (eval `{1 + 2}) "parse: operacion aritmetica no es prefix.")
(test/exn (eval `{+ 1 2 3}) "parse: operacion aritmetica malformada.")
(test/exn (eval `{/ 5 5}) "parse: no se reconoce el simbolo en la operacion aritmetica.")