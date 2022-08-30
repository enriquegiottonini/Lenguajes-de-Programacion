#lang racket

;;
;; 2.1 Recursion sin estructura
;;


;; unit-string? : String -> Bool
(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

;; unit-string-list? : (listof unit-string?) -> Bool
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (unit-string? (first x))
           (unit-string-list? (rest x)))))

;; explode : String -> (listof unit-string?)
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibi: ~e" s))
  (map string (string->list s)))

;; implode : (listof unit-string?) -> String
(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibi: ~e" ls))
  (apply string-append ls))

;; bundle : (listof unit-string?) -> (listof string?)
;; empaqueta trozos de una lista de cadenas unitarias 's' en cadenas de size 'n'
(define (bundle s n)
  (unless (unit-string-list? s)
    (error 'bundle "esperaba una lista de cadenas unitarias, pero recibi ~e" s))
  (unless (exact-nonnegative-integer? n)
    (error 'bundle "esperaba un entero positivo exacto (sin decimal), pero recibi ~e" n))
  (unless (not (zero? n))
    (error 'bundle "n no puede ser 0."))
  (map implode (list->chunks s n)))

;; PROBLEMA 3
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

;; PROBLEMA 6
;; list->chunks : List Number -> List
;; Consume una lista l de valores arbitrarios y un
;; natural n . El resultado es una lista de trozos de tamaño n .
;; Cada trozo representa una sub-secuencia
;; de elementos en l .
(define (list->chunks l n)
  (unless (list? l)
    (error 'list->chunks "esperaba una lista, recibi ~e" l))
  (unless (exact-nonnegative-integer? n)
    (error 'list->chunks "esperaba un numero natural, recibi ~e" n))
  (unless (not (zero? n))
    (error 'list->chunks "los chunks no pueden ser de size 0"))
  (cond
    [(null? l) l]
    [else
     (cons (apply list (take l n))
           (list->chunks (drop l n) n))]))

;; PROBLEMA 7
;; partition : String Number -> ListOfStrings
;; Toma una cadena s y un natural n. Produce una lista de trozos de
;; cadenas de tamaño n.
(define (partition s n)
  (unless (string? s)
    (error 'partition "esperaba una cadena de caracteres, recibi ~e" s))
  (unless (exact-nonnegative-integer? n)
    (error 'partition "esperaba un numero natural, recibi ~e" n))
  (unless (not (zero? n))
    (error 'partition "las particiones no pueden ser de size 0"))
  (define (partition-in-range str step max)
    (cond
      [(>= n max) (cons str '())]
      [else
       (cons (substring str 0 step)
             (partition-in-range (substring str step) step (- max step)))]))
  (partition-in-range s n (string-length s)))

;;
;; 2.2 Recursion que ignora estructura
;;

(define (isort ls cmp)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) cmp) cmp)))

;; PROBLEMA 8
(define (insert n ls cmp)
  (cond
    [(empty? ls) (list n)]
    [(cmp n (first ls)) (cons n ls)]
    [else
     (cons (first ls) (insert n (rest ls) cmp))]))

;; PROBLEMA 9

;; quicksort : List Proc -> List
(define (quicksort ls cmp)
  (unless (list? ls) (error 'quicksort "esperaba una lista, recibi ~e" ls))
  (unless (procedure? cmp) (error 'quicksort "esperaba un procedimiento, recibi ~e" cmp))
  (cond
    [(empty? ls) null]
    [(< (length ls) 100) (isort ls cmp)]
    [else
     (define pivot (first ls))
     (define smallers (filter (lambda (x) (cmp x pivot)) ls))
     (define largers  (filter (lambda (x) (and (not (cmp x pivot)) (not (equal? x pivot)))) ls))
     (define equals   (filter (lambda (x) (equal? x pivot)) ls))
     (append (quicksort smallers cmp)
             equals
             (quicksort largers cmp))]))

;; 2.5 Tomando decicisones
(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          (else (find-largest-divisor (- k 1)))))
  (find-largest-divisor (min n m)))

(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

(provide (all-defined-out))