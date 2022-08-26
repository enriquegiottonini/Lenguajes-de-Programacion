#lang racket

;; 2.1 Recursion sin estructura

(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (unit-string? (first x))
           (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibi: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibi: ~e" ls))
  (apply string-append ls))

;; bundle : ListOfUnitStrings -> ListOfStrings
;; empaqueta trozos de una lista de cadenas unitarias 's' en cadenas de size 'n'
(define (bundle lst n)
  (unless (unit-string-list? lst)
    (error 'bundle "esperaba una lista de cadenas unitarias, pero recibi ~e" lst))
  (unless (exact-nonnegative-integer? n)
    (error 'bundle "esperaba un entero positivo exacto (sin decimal), pero recibi ~e" n))
  '())

;; PROBLEMA 3:

;; take: List Number -> List
;; regresa una lista de los primeros n elementos
;; de l , o cuantos elementos haya si l tiene menos de n elementos.
(define (take l n)
  (unless (list? l)
    (error 'take "esperaba una lista, recibi ~e" l))
  (unless (exact-nonnegative-integer? n)
    (error 'take "esperaba un numero natural o 0, recibi ~e" n))
  (cond
    [(empty? l) '()]
    [(zero? n) '()]
    [else
     (cons (car l) (take (rest l) (sub1 n)))]))

;; drop: List Number -> List
;; regresa una lista con los elementos de l
;; excepto los primeros n , o la cadena vacía si l tiene menos de n elementos.
(define (drop l n)
  (unless (list? l)
    (error 'take "esperaba una lista, recibi ~e" l))
  (unless (exact-nonnegative-integer? n)
    (error 'take "esperaba un numero natural o 0, recibi ~e" n))
  (cond
    [(empty? l) '()]
    [(zero? n) l]
    [else
     (drop (rest l) (sub1 n))]))

(provide (all-defined-out))