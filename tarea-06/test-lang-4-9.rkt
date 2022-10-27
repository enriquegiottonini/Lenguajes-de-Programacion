#lang racket

(require rackunit
         rackunit/text-ui
         "lang-4-9.rkt")

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
                           (computation (num-val 1) (store '#(0 0 0 0 0 0 0 0 0 0) 0))))
  ;; el punto es que value-of ahora regresa una estructura que representa el computo
  ;; y tiene 2 hijos, el valor de computo y el estado de almacenamiento.
  (test-case "diff-exp"
             (check-equal? (run `(- 2 1))
                           (computation (num-val 1) (store '#(0 0 0 0 0 0 0 0 0 0) 0))))

  (test-case "newref-exp"
             (check-equal? (run `(newref 10))
                           (computation (ref-val 0) (store (vector (num-val 10) 0 0 0 0 0 0 0 0 0) 1)))
             (check-equal? (run `(let (x (newref 6))
                                   (let (y (newref 6))
                                     (let (z (newref 6))
                                       (newref 9)))))
                           (computation (ref-val 3) (store (vector (num-val 6) (num-val 6) (num-val 6) (num-val 9) 0 0 0 0 0 0) 4)))
             (check-equal? (run `(let (a (newref 1))
                                   (let (b (newref 2))
                                     (let (c (newref 3))
                                       (let (d (newref 4))
                                         (let (e (newref 5))
                                           (let (f (newref 6))
                                             (let (g (newref 7))
                                               (let (h (newref 8))
                                                 (let (i (newref 9))
                                                   (let (j (newref 10))
                                                     (let (k (newref 11))
                                                       (let (k (newref 12)) 3)))))))))))))
                           (computation (num-val 3)
                                        (store
                                         (vector (num-val 1) (num-val 2) (num-val 3) (num-val 4) (num-val 5) (num-val 6)
                                                 (num-val 7) (num-val 8) (num-val 9) (num-val 10) (num-val 11) (num-val 12) 0 0 0 0 0 0 0 0)
                                         12)))
             (check-equal? (run `(let (x (newref 2))
                                   (let (y (newref 2))
                                     (- x y))))
                           (computation (num-val -1)
                                        (store (vector (num-val 2) (num-val 2) 0 0 0 0 0 0 0 0) 2))))

  (test-case "deref-exp"
             (check-equal? (run `(deref 0))
                                        (computation 0 (store '#(0 0 0 0 0 0 0 0 0 0) 0)))
             (check-exn exn:fail? (thunk (run `(deref -1))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-equal? (run `(let (x (newref 9))
                                   (deref 0)))
                           (computation (num-val 9) (store (vector (num-val 9) 0 0 0 0 0 0 0 0 0) 1)))
             (check-equal? (run `(let (x (newref 9))
                                   (let (y (newref 10))
                                   (deref 1))))
                           (computation (num-val 10) (store (vector (num-val 9) (num-val 10) 0 0 0 0 0 0 0 0) 2))))

  (test-case "setref-exp"
             (check-exn exn:fail? (thunk (run `(setref 0))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-exn exn:fail? (thunk (run `(setref -1))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-equal? (run `(let (x (newref 6))
                                   (setref x 9)))
                           (computation (num-val 9) (store (vector (num-val 9) 0 0 0 0 0 0 0 0 0) 1)))
             (check-equal? (run `(let (x (newref 9))
                                   (let (y (newref 10))
                                     (setref y 20))))
                           (computation (num-val 20) (store (vector (num-val 9) (num-val 20) 0 0 0 0 0 0 0 0) 2)))
             )
  
  )
(run-tests pruebas 'verbose)
