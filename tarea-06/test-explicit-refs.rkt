#lang racket

(require rackunit
         rackunit/text-ui
         "explicit-refs.rkt")

(define-test-suite pruebas
  (test-case "parser"
    (check-equal? (parse `(newref 0))
                  (a-program (newref-exp (const-exp 0))))
    (check-equal? (parse `(deref (newref 0)))
                  (a-program (deref-exp (newref-exp (const-exp 0)))))
    (check-equal? (parse `(setref some-loc some-val))
                  (a-program (setref-exp (var-exp 'some-loc) (var-exp 'some-val)))))

  (test-case "const-exp"
             (check-equal? (run `1)
                           (computation (num-val 1) '())))
  ;; el punto es que value-of ahora regresa una estructura que representa el computo
  ;; y tiene 2 hijos, el valor de computo y el estado de almacenamiento.
  (test-case "diff-exp"
             (check-equal? (run `(- 2 1))
                           (computation (num-val 1) '())))

  (test-case "newref-exp"
             (check-equal? (run `(newref 10))
                           (computation (ref-val 0) (list (num-val 10))))
             (check-equal? (run `(let (x (newref 6))
                                   (let (y (newref 6))
                                     (let (z (newref 6))
                                       (newref 9)))))
                           (computation (ref-val 3) (list (num-val 6)
                                                          (num-val 6)
                                                          (num-val 6)
                                                          (num-val 9))))
             (check-equal? (run `(let (x (newref 2))
                                   (let (y (newref 2))
                                     (- x y))))
                           (computation (num-val -1) (list (num-val 2) (num-val 2)))))

  )
(run-tests pruebas 'verbose)
