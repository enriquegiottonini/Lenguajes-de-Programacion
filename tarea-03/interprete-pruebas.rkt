#lang plait

(require "interprete.rkt")
(print-only-errors #t)

;; testing lookup
(test/exn (lookup 'a empty-env) "unbound identifier 'a")
(test/exn (lookup 'c (list (bind 'a (numV 1))
                           (bind 'b (numV 2))))
          "unbound identifier 'c")
(test (lookup 'a (list (bind 'a (numV 5))))
      (numV 5))
(test (lookup 'a (list (bind 'b (strV "foo"))
                       (bind 'a (boolV #t))
                       (bind 'a (numV 1))))
      (boolV #t))

