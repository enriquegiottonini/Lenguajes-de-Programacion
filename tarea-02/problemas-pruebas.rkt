#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "bundle"
    (check-exn exn:fail? (thunk (bundle (list "a" "b" "c") -1)))
    (check-exn exn:fail? (thunk (bundle (list "a" "b" "c") 1.5)))
    (check-exn exn:fail? (thunk (bundle (list "ab" "bc" "cd") 1)))
    (check-equal? (bundle (explode "abcdefg") 3)
                  (list "abc" "def" "g"))
    (check-equal? (bundle '() 2)
                  '())
    (check-equal? (bundle (explode "abcdefg") 0)
                  '())
    (check-equal? (bundle (explode "abcdefg") 1)
                  (list "a" "b" "c" "d" "e" "f" "g")))

  (test-case "take"
    (check-exn exn:fail? (thunk (take (list "a" "b" "c") -1)))
    (check-exn exn:fail? (thunk (take (list "a" "b" "c") 1.4)))
    (check-exn exn:fail? (thunk (take "ok" 3)))
    (check-equal? (take (list "1" "2" "3") 0)
                  '())
    (check-equal? (take (list "1" "2" "3") 1)
                  (list "1"))
    (check-equal? (take (list "1" "2" "3") 10)
                  (list "1" "2" "3")))
  
  (test-case "drop"
    (check-exn exn:fail? (thunk (take (list "a" "b" "c") -1)))
    (check-exn exn:fail? (thunk (take (list "a" "b" "c") 1.4)))
    (check-exn exn:fail? (thunk (take "ok" 3)))
    (check-equal? (drop (list "1" "2" "3") 0)
                  (list "1" "2" "3"))
    (check-equal? (drop (list "1" "2" "3") 1)
                  (list "2" "3"))
    (check-equal? (drop (list "1" "2" "3") 2)
                  (list "3"))
    (check-equal? (drop (list "1" "2" "3") 10)
                  '())))


    
(run-tests pruebas 'verbose)