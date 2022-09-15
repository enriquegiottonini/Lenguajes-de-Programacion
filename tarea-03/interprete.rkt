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
  (binopC (op : binops) [l : ExprC] [r : ExprC])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (funC [name : Symbol] [body : ExprC])
  (appC [id : ExprC] [value : ExprC]))

;; Como minimo contiene a ExprC, agrega and, or, let
;; usar binops para +, ++, ==
(define-type ExprS
  (numS [n : Number])
  (strS [s : String])
  (idS [name : Symbol])
  (boolS [b : Boolean])
  (binopS [op : binops] [l : ExprS] [r : ExprS])
  (plusS [l : ExprS] [r : ExprS])
  (concatS [l : ExprS] [r : ExprS])
  (numeqS [l : ExprS] [r : ExprS])
  (streqS [l : ExprS] [r : ExprS])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (funS [name : Symbol] [body : ExprS])
  (appS [id : ExprS] [value : ExprS]))

(define-type binops
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

;; interp : ExprC -> Value
;; post-orden
(define (interp [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    [(numC n) (numV n)]
    [(strC s) (strV s)]
    [(idC name) (lookup name env)]
    [(boolC b) (boolV b)]
    [(binopC op l r) (let ([rval (interp r env)]
                           [lval (interp l env)])
                       (let ([rtype (get-variant-value rval)]
                             [ltype (get-variant-value lval)]
                             [optype (get-op-symbol op)])
                         (if (eq? rtype ltype)
                             (cond
                               [(and (plusO? op) (numV? rval))
                                (numV (+ (numV-n lval) (numV-n rval)))]
                               [(and (numeqO? op) (numV? rval))
                                (boolV (= (numV-n lval) (numV-n rval)))]
                               [(and (appendO? op) (strV? rval))
                                (strV (string-append (strV-s lval) (strV-s rval)))]
                               [(and (streqO? op) (strV? rval))
                                (boolV (string=? (strV-s lval) (strV-s rval)))]
                               [else (error 'interp "bad operands.")])
                             (error-typecheck-binop optype ltype rtype))))]
                               
                           
[else (numV 0)]))

(define (get-variant-value [v : Value]) : Symbol
  (cond
    [(numV? v) 'numV]
    [(boolV? v) 'boolV]
    [(strV? v) 'strV]
    [(procV? v) 'procV]))

(define (get-op-symbol [op : binops]) : Symbol
  (cond
    [(plusO? op) '+]
    [(numeqO? op) 'num=]
    [(appendO? op) '++]
    [(streqO? op) 'str=]))                   

;; ENVIRONMENTS
;; hacer pruebas con hash mutable e inmutable
(define-type Binding
  (bind [id : Symbol] [value : Value]))
(define-type-alias Environment (Listof Binding))
(define empty-env empty)
(define extend-env cons)

;; lookup
(define (lookup [name : Symbol] (env : Environment)) : Value
  (if (empty? env)
      (error-unbound-id name)
      (if (eq? name (bind-id (first env)))
          (bind-value (first env))
          (lookup name (rest env)))))
                             
;; desugar : ExprS -> ExprC


;;(define (eval [str : S-Exp]) : Value
;;  (interp (desugar (parse str))))

;; Entornos : Symbol(id) -> Value
;; usar hash inmutables y luego mutables?

;; Funciones unarias con lexical scope

;; ERRORES
(define (error-bad-if-conditional [val : String])
  (error 'interp
         (string-append
          val " no es un argumento booleano.")))

(define (error-typecheck-binop [op : Symbol] [val1 : Symbol] [val2 : Symbol])
  (error 'interp
         (write (list
                 "type-check failed,"
                 (symbol->string val1)
                 "vs."
                 (symbol->string val2)
                 "in"
                 (symbol->string op)))))

(define (write [l : (Listof String)])
  (if (empty? l)
      ""
      (string-append (first l)
                     (string-append " " (write (rest l))))))
            

(define (error-unbound-id [id : Symbol])
  (error 'interp
         (string-append
          "unbound identifier '"
          (symbol->string id))))

(define (error-bad-proc [name : String])
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