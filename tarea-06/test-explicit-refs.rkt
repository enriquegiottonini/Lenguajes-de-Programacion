#lang racket

(require rackunit
         rackunit/text-ui
         "explicit-refs.rkt")

(define-test-suite parser
  (test-case "construcciones"
    (check-equal? (parse `(newref 0))
                  (a-program (newref-exp (const-exp 0))))
    (check-equal? (parse `(deref (newref 0)))
                  (a-program (deref-exp (newref-exp (const-exp 0)))))
    (check-equal? (parse `(setref some-loc some-val))
                  (a-program (setref-exp (var-exp 'some-loc) (var-exp 'some-val))))))



(run-tests parser 'verbose)