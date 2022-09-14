#lang plait

(define-type Value
  (numV  [n : Number])
  (boolV [b : Boolean])
  (strV  [s : String])
  (procV [f : (Symbol -> Value)]))

(define-type ExprC
  (numC [n : Number])
  (strC [s : String])
  (idC [name : Symbol])
  (boolC [b : Boolean])
  (plusC [l : ExprC] [r : ExprC])
  (concatC [l : ExprC] [r : ExprC])
  (numeqC [l : ExprC] [r : ExprC])
  (streqC [l : ExprC] [r : ExprC])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (funC [name : Symbol] [body : ExprC])
  (callC [name : Symbol] [value : ExprC]))

;; Como minimo contiene a ExprC, agrega and, or, let
;; usar binops para +, ++, ==
(define-type ExprS
  (numS [n : Number])
  (strS [s : String])
  (idS [name : Symbol])
  (boolS [b : Boolean])
  (binopS [l : ExprS] [r : ExprS])
  (plusS [l : ExprS] [r : ExprS])
  (concatS [l : ExprS] [r : ExprS])
  (numeqS [l : ExprS] [r : ExprS])
  (streqS [l : ExprS] [r : ExprS])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (funS [name : Symbol] [body : ExprS])
  (callS [name : Symbol] [value : ExprS]))

;; desugar : ExprS -> ExprC

;; interp : ExprC -> Value

;;(define (eval [str : S-Exp]) : Value
;;  (interp (desugar (parse str))))

;; Entornos : Symbol(id) -> Value
;; usar hash inmutables y luego mutables?

;; Funciones unarias con lexical scope

;; ERRORES
(define (error-bad-if-conditional [val : String]) : Void
  (error 'interp
         (string-append
          val " no es un argumento booleano.")))

(define (error-bad-operand val1 val2)
  (error 'interp
         (string-append
          "type val1 vs type val2"
          "malo")))

(define (error-unbound-id [id : String]) : Void
  (error 'interp
         (string-append
          "unbound identifier "
          id)))

(define (error-bad-proc [name : String]) : Void
  (error 'interp
         (string-append
          name " not a function.")))

(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in)
     (parse-number in)]
    [(s-exp-string? in)
     (parse-string in)]
    [(s-exp-match? `true in)
     (boolS #t)]
    [(s-exp-match? `false in)
     (boolS #f)]
    [(s-exp-match? `{if ANY ...} in)
     (parse-if in)]
    [(s-exp-match? `{and ANY ...} in)
     (parse-and in)]
    [(s-exp-match? `{or ANY ...} in)
     (parse-or in)]
    [(s-exp-match? `{+ ANY ...} in)
     (parse-+ in)]
    [(s-exp-match? `{++ ANY ...} in)
     (parse-++ in)]
    [(s-exp-match? `{num= ANY ...} in)
     (parse-num= in)]
    [(s-exp-match? `{str= ANY ...} in)
     (parse-str= in)]
    [(s-exp-match? `{fun ANY ...} in)
     (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
    [(s-exp-match? `{ANY ...} in)
     (parse-app in)]
    [(s-exp-symbol? in)
     (parse-id in)]))

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
         (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de
               ↩→
               funciones"))))