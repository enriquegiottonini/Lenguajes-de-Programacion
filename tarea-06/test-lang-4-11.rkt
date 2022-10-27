#lang racket

(require rackunit
         rackunit/text-ui
         "lang-4-11.rkt")

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
                           (computation (num-val -1) (list (num-val 2)
                                                           (num-val 2)))))

  (test-case "deref-exp"
             (check-exn exn:fail? (thunk (run `(deref 0))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-exn exn:fail? (thunk (run `(deref -1))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-equal? (run `(let (x (newref 9))
                                   (deref 0)))
                           (computation (num-val 9) (list (num-val 9))))
             (check-equal? (run `(let (x (newref 9))
                                   (let (y (newref 10))
                                     (deref y))))
                           (computation (num-val 10) (list (num-val 9) (num-val 10)))))

  (test-case "setref-exp"
             (check-exn exn:fail? (thunk (run `(setref 0))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-exn exn:fail? (thunk (run `(setref -1))
                                         "deref-exp: no se puede encontrar la locación 0"))
             (check-equal? (run `(let (x (newref 6))
                                   (setref x 9)))
                           (computation (num-val 9) (list  (num-val 9)))))
  (test-case "list-exp"
             (check-equal? (run `(list ((emptylist null))))
                           (pair-val (cons (computation '() '()) (null-val '()))))
             (check-equal? (run `(list ((- 2 1) (- 3 2) (- 2 2))))
                           (pair-val (cons (computation (num-val 1) '()) (pair-val (cons (computation (num-val 1) '()) (pair-val (cons (computation (num-val 0) '()) (null-val '()))))))))
             (check-equal? (run `(list ((newref 69) (deref 0) (setref 0 36) (deref 0) (newref 10) (deref 1))))
                           (pair-val (cons (computation (ref-val 0) (list (num-val 69)))
                                           (pair-val (cons (computation (num-val 69) (list (num-val 69)))
                                                           (pair-val (cons (computation (num-val 36) (list (num-val 36)))
                                                                           (pair-val (cons (computation (num-val 36) (list (num-val 36)))
                                                                                           (pair-val (cons (computation (ref-val 1) (list (num-val 36) (num-val 10)))
                                                                                                           (pair-val (cons (computation (num-val 10) (list (num-val 36) (num-val 10)))
                                                                                                                           (null-val '()))))))))))))))
             (check-equal? (run `(list ((emptylist null) (- 6 6) (- 6 -1))))
                           (pair-val (cons (computation '() '()) (pair-val (cons (computation (num-val 0) '()) (pair-val (cons (computation (num-val 7) '()) (null-val '()))))))))))
  
(run-tests pruebas 'verbose)

