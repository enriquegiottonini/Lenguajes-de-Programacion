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

;; testing interp-h binopC
(test (interp-h (binopC (plusO) (numC 1) (numC 0)) empty-env)
      (numV 1))
(test (interp-h (binopC (appendO) (strC "ok") (strC "google")) empty-env)
      (strV "okgoogle"))
(test (interp-h (binopC (numeqO) (numC 1) (numC 1)) empty-env)
      (boolV #t))
(test (interp-h (binopC (numeqO) (numC 1) (numC 0)) empty-env)
      (boolV #f))
(test (interp-h (binopC (streqO) (strC "ok") (strC "ok")) empty-env)
      (boolV #t))
(test (interp-h (binopC (streqO) (strC "ok") (strC "google")) empty-env)
      (boolV #f))
(test (interp-h (binopC (streqO) (strC "ok") (strC "ok ")) empty-env)
      (boolV #f))
(test/exn (interp-h (binopC (plusO) (numC 1) (boolC #t)) empty-env)
          "interp-h: type-check failed, numV vs. boolV in + ")
(test/exn (interp-h (binopC (appendO) (strC "ok") (boolC #t)) empty-env)
          "interp-h: type-check failed, strV vs. boolV in ++ ")
(test/exn (interp-h (binopC (numeqO) (numC 1) (strC "ok")) empty-env)
          "interp-h: type-check failed, numV vs. strV in num= ")
(test/exn (interp-h (binopC (streqO) (numC 1) (strC "ok")) empty-env)
          "interp-h: type-check failed, numV vs. strV in str= ")
(test/exn (interp-h (binopC (plusO) (strC "ok") (strC "ok")) empty-env)
          "interp-h: bad operands in ( + strV strV ) ")
(test/exn (interp-h (binopC (appendO) (numC 1) (numC 1)) empty-env)
          "interp-h: bad operands in ( ++ numV numV ) ")
(test/exn (interp-h (binopC (numeqO) (strC "ok") (strC "ok")) empty-env)
          "interp-h: bad operands in ( num= strV strV ) ")
(test/exn (interp-h (binopC (streqO) (numC 1) (numC 1)) empty-env)
          "interp-h: bad operands in ( str= numV numV ) ")

;; interp-h ifC
(test (interp-h (ifC (boolC #t)
                   (binopC (plusO) (numC 1) (numC 2))
                   (binopC (appendO) (strC "ok") (strC "google"))) empty-env)
      (numV 3))
(test (interp-h (ifC (boolC #t)
                   (binopC (plusO) (numC 1) (numC 2))
                   (binopC (appendO) (idC 'x) (idC 'y))) empty-env)
      (numV 3))
(test (interp-h (ifC (boolC #f)
                   (binopC (plusO) (numC 1) (numC 2))
                   (binopC (appendO) (strC "ok") (strC "google"))) empty-env)
      (strV "okgoogle"))
(test/exn (interp-h (ifC (numC 1)
                       (binopC (plusO) (numC 1) (numC 2))
                       (binopC (appendO) (strC "ok") (strC "google"))) empty-env)
          "interp-h: bad conditional numV ")

;; interp-h fun

(test (interp-h (funC 'x (binopC (plusO) (idC 'x) (idC 'x))) empty-env)
      (funV 'x (binopC (plusO) (idC 'x) (idC 'x))))

(test (interp-h (funC 'x (binopC (plusO) (idC 'x) (idC 'x)))
              (list (bind 'x (numV 10))))
      (funV 'x (binopC (plusO) (idC 'x) (idC 'x))))

;; interp-h apply function

(test (interp-h (appC
               (funC 'x (binopC (plusO) (idC 'x) (idC 'x)))
               (numC 10))
              empty-env)
      (numV 20))

(test (interp-h (appC
               (funC 'x (binopC (plusO) (idC 'x) (idC 'x)))
               (numC 10))
              (list (bind 'x (numV 100))))
      (numV 20))

(test (interp-h (appC (funC 'x (appC
                              (funC 'y (binopC (plusO) (idC 'y) (idC 'y)))
                              (idC 'x)))
                    (numC 10))
              empty-env)
      (numV 20))

(test/exn (interp-h (appC (idC 'f) (numC 10))
                  empty-env)
          "interp-h: unbound identifier 'f")


