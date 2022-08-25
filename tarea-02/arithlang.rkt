#lang plait

;; lenguaje aritmetico Core
;; ejemplo: (define foo (numC 9)
;;          (define bar (plusC (multC (numC 1) (numC 2)) (numC 3)))
(define-type ArithC
  (numC [n : Number])
  (plusC [l : ArithC] [r : ArithC])
  (multC [l : ArithC] [r : ArithC]))

;; lenguaje aritmetico extendido, azucarado, Superficial
;; ejemplo: (define foo (numS 1)
;;          (define bar (plusS (numS 1) (bminus (multS (numS 2) (numS4)))))
(define-type ArithS
  (numS [n : Number])
  (uminusS [e : ArithS])
  (plusS [l : ArithS] [r : ArithS])
  (bminusS [l : ArithS] [r : ArithS])
  (multS [l : ArithS] [r : ArithS]))

;; evaluador de una expresion aritmetica  bien formada
;; que regresa su valor numerico.
;; ejemplo:
;; > (eval `{+ 1 2})
;; - Number
;; 3
(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

;; interpreta una expresion en lenguaje core
;; y regresa su valor numerico.
;; ejemplo:
;; > (interp (numC 1))
;; - Number
;; 1
(define (interp  [ac : ArithC]) : Number
  (cond
    [(numC? ac) (numC-n ac)]
    [(plusC? ac) (+ (interp (plusC-l ac)) (interp (plusC-r ac)))]
    [(multC? ac) (* (interp (multC-l ac)) (interp (multC-r ac)))]))
    

;; convierte una expresion en lenguaje extendido a
;; una expresion en lenguaje core.
;; ejemplo:
;; > (desugar (numS 1))
;; - ArithC
;; (numC 1)
(define (desugar [as : ArithS]) : ArithC
  (cond
    [(numS? as) (numC (numS-n as))]
    [(uminusS? as) (multC (numC -1) (desugar (uminusS-e as)))]
    [(plusS? as) (plusC (desugar (plusS-l as)) (desugar (plusS-r as)))]
    [(bminusS? as) (plusC (desugar (bminusS-l as)) (multC (numC -1) (desugar (bminusS-r as))))]
    [(multS? as) (multC (desugar (multS-l as)) (desugar (multS-r as)))]))

;; convierte una expresion a una expresion
;; aritmetica extendida, azucarada.
;; ejemplo:
;; > (parse `{+ 1 2})
;; - ArithS
;; (plusS (numS 1) (numS 2))
(define (parse [s : S-Exp]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(and (s-exp-list? s) (not (equal? s `{})))
     (let ([lst (s-exp->list s)])
       (cond
         [(not (s-exp-symbol? (first lst))) (error 'parse "operacion aritmetica no es prefix.")]
         [(= (length lst) 2)
          (case (s-exp->symbol (first lst))
            ['- (uminusS (parse (second lst)))])]
         [(= (length lst) 3)
          (case (s-exp->symbol (first lst))
            ['+ (plusS (parse (second lst)) (parse (third lst)))]
            ['* (multS (parse (second lst)) (parse (third lst)))]
            ['- (bminusS (parse (second lst)) (parse (third lst)))]
            [else (error 'parse "no se reconoce el simbolo en la operacion aritmetica.")])]
         [else (error 'parse "operacion aritmetica malformada.")]))]
     [else (error 'parse "expresion aritmetica malformada.")]))
