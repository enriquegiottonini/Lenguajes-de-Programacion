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

;; testing interp binopC
(test (interp (binopC (plusO) (numC 1) (numC 0)) empty-env)
      (numV 1))
(test (interp (binopC (appendO) (strC "ok") (strC "google")) empty-env)
      (strV "okgoogle"))
(test (interp (binopC (numeqO) (numC 1) (numC 1)) empty-env)
      (boolV #t))
(test (interp (binopC (numeqO) (numC 1) (numC 0)) empty-env)
      (boolV #f))
(test (interp (binopC (streqO) (strC "ok") (strC "ok")) empty-env)
      (boolV #t))
(test (interp (binopC (streqO) (strC "ok") (strC "google")) empty-env)
      (boolV #f))
(test (interp (binopC (streqO) (strC "ok") (strC "ok ")) empty-env)
      (boolV #f))
(test/exn (interp (binopC (plusO) (numC 1) (boolC #t)) empty-env)
          "bad operands.")