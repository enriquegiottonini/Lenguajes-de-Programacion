#lang racket

(require rackunit
         rackunit/text-ui
         "explicit-refs.rkt")

(define-test-suite trivial
  (test-case "testeando lo trivial"
    (check-equal? #t #t)
    (check-equal? #f #f)))

(run-tests trivial 'verbose)