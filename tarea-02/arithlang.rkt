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
  (type-case ArithC ac
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))
    

;; convierte una expresion en lenguaje extendido a
;; una expresion en lenguaje core.
;; ejemplo:
;; > (desugar (numS 1))
;; - ArithC
;; (numC 1)
(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [(numS n) (numC n)]
    [(uminusS e) (multC (numC -1) (desugar e))]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(bminusS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [(multS l r) (multC (desugar l) (desugar r))]))

;; convierte una expresion-S a una expresion
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
         [(not (s-exp-symbol? (first lst))) (malformed-sexp-err "")]
         [(= (length lst) 2)
          (case (s-exp->symbol (first lst))
            ['- (uminusS (parse (second lst)))]
            [else (malformed-sexp-err "")])]
         [(= (length lst) 3)
          (case (s-exp->symbol (first lst))
            ['+ (plusS (parse (second lst)) (parse (third lst)))]
            ['* (multS (parse (second lst)) (parse (third lst)))]
            ['- (bminusS (parse (second lst)) (parse (third lst)))]
            [else (malformed-sexp-err "")])]
         [else (malformed-sexp-err "")]))]
     [else (malformed-sexp-err "")]))

(define (malformed-sexp-err [msg : String])
  (if (> 0 (string-length msg))
      (error 'parse msg)
      (error 'parse "expresion malformada.")))