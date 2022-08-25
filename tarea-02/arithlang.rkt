#lang plait

(define-type ArithC
  (numC [n : Number])
  (plusC [l : ArithC] [r : ArithC])
  (multC [l : ArithC] [r : ArithC]))

(define-type ArithS
  (numS [n : Number])
  (uminusS [e : ArithS])
  (plusS [l : ArithS] [r : ArithS])
  (bminusS [l : ArithS] [r : ArithS])
  (multS [l : ArithS] [r : ArithS]))

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

(define (interp [ac : ArithC]) : Number
  (cond
    [(numC? ac) (numC-n ac)]
    [(plusC? ac) (+ (interp (plusC-l ac)) (interp (plusC-r ac)))]
    [(multC? ac) (* (interp (multC-l ac)) (interp (multC-r ac)))]))
    

(define (desugar [as : ArithS]) : ArithC
  (cond
    [(numS? as) (numC (numS-n as))]
    [(uminusS? as) (multC (numC -1) (desugar (uminusS-e as)))]
    [(plusS? as) (plusC (desugar (plusS-l as)) (desugar (plusS-r as)))]
    [(bminusS? as) (plusC (desugar (bminusS-l as)) (multC (numC -1) (desugar (bminusS-r as))))]
    [(multS? as) (multC (desugar (multS-l as)) (desugar (multS-r as)))]))

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
            ['- (bminusS (parse (second lst)) (parse (third lst)))])]
         [else (error 'parse "operacion aritmetica malformada.")]))]
     [else (error 'parse "expresion aritmetica malformada.")]))