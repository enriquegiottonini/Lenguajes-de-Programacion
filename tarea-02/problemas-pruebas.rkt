#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "bundle"
    (check-exn exn:fail? (thunk (bundle (list "a" "b" "c") -1)))
    (check-exn exn:fail? (thunk (bundle (list "a" "b" "c") 1.5)))
    (check-exn exn:fail? (thunk (bundle (list "ab" "bc" "cd") 1)))
    (check-exn exn:fail? (thunk (bundle (list "ab" "bc" "cd") 0)))
    (check-equal? (bundle (explode "abcdefg") 3)
                  (list "abc" "def" "g"))
    (check-equal? (bundle (explode "abc") 5)
                  (list "abc"))
    (check-equal? (bundle '() 2)
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
                  '()))

  (test-case "list->chunks"
     (check-exn exn:fail? (thunk (list->chunks (list 1 2 3) -1)))
     (check-exn exn:fail? (thunk (list->chunks (list 1 2 3) 1.5)))
     (check-exn exn:fail? (thunk (list->chunks (list 1 2 3) 0)))
     (check-exn exn:fail? (thunk (list->chunks 4 1)))
     (check-equal? (list->chunks (list 1 2 "ok" (list 9 9)) 2)
                   '( (1 2) ("ok" (9 9) )))
     (check-equal? (list->chunks (list 1 2 "ok" (list 9 9)) 10)
                   '( (1 2 "ok" (9 9)) )))

  (test-case "partition"
    (check-exn exn:fail? (thunk (partition "abc" -1)))
    (check-exn exn:fail? (thunk (partition "abc" 1.5)))
    (check-exn exn:fail? (thunk (partition (list "ab" "bc" "cd") 1)))
    (check-exn exn:fail? (thunk (partition "abc" 0)))
    (check-equal? (partition "abcdefg" 3)
                  (bundle (explode "abcdefg") 3))       
    (check-equal? (partition "abc" 5)
                  (bundle (explode "abc") 5))
    (check-equal? (partition  "" 9)
                  '(""))
    (check-equal? (partition "abcdefg" 1)
                  (list "a" "b" "c" "d" "e" "f" "g")))

  (test-case "smallers"
    (check-exn exn:fail? (thunk (smallers (list 1 4 2 5) "s")))
    (check-exn exn:fail? (thunk (smallers "adf" 4)))
    (check-equal? (smallers null 4)
                  null)
    (check-equal? (smallers (list 6 7 8 9) 4)
                  null)
    (check-equal? (smallers (list 6 7 8 9) 9)
                  (list 6 7 8))
    (check-equal? (smallers (list 6 7 8 9) 100)
                  (list 6 7 8 9)))

  (test-case "largers"
    (check-exn exn:fail? (thunk (largers (list 1 4 2 5) "s")))
    (check-exn exn:fail? (thunk (largers "adf" 4)))
    (check-equal? (largers null 4)
                  null)
    (check-equal? (largers (list 6 7 8 9) 4)
                  (list 6 7 8 9))
    (check-equal? (largers (list 6 7 8 9 10) 9)
                  (list 10))
    (check-equal? (largers (list 6 7 8 9) 100)
                  null))
             
             
  )

(run-tests pruebas 'verbose)