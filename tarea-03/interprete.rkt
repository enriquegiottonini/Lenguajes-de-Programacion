#lang plait
;; Value
(define-type Value
  (numV  [n : Number])
  (boolV [b : Boolean])
  (strV  [s : String])
  (funV [param : Symbol] [body : ExprC] [fenv : Environment]))

;; ExprC
(define-type ExprC
  (numC [n : Number])
  (strC [s : String])
  (idC [name : Symbol])
  (boolC [b : Boolean])
  (binopC (op : binops) [l : ExprC] [r : ExprC])
  (ifC [cond : ExprC] [conseq : ExprC] [alt : ExprC])
  (funC [param : Symbol] [body : ExprC])
  (appC [procedure : ExprC] [arg : ExprC]))

(define-type binops
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

;; interp-h : ExprC -> Value
(define (interp-h [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    [(numC n) (numV n)]
    [(strC s) (strV s)]
    [(idC name) (lookup name env)]
    [(boolC b) (boolV b)]
    [(binopC op l r) (interp-h-binopC op l r env)]
    [(ifC cond conseq alt) (let ([cndval (interp-h cond env)])
                             (if (boolV? cndval)
                                 (if (boolV-b cndval)
                                     (interp-h conseq env)
                                     (interp-h alt env)) 
                                 (error-bad-conditional-simple)))]
    [(funC param body) (funV param body env)]
    [(appC procedure arg) (let ([fun (interp-h procedure env)])
                            (if (funV? fun)
                                (let ([param (funV-param fun)]
                                      [body  (funV-body fun)]
                                      [fenv (funV-fenv fun)])
                                  (interp-h body (extend-env (bind param (interp-h arg env)) fenv)))
                                (error-bad-proc-simple)))]))

(define (interp-h-binopC [op : binops] [l : ExprC] [r : ExprC] [env : Environment])
  (let ([lval (interp-h l env)]
        [rval (interp-h r env)])
    (let ([ltype (get-variant-value lval)]
          [rtype (get-variant-value rval)]
          [optype (get-op-symbol op)])
      (if (eq? rtype ltype)
          (cond
            [(and (plusO? op) (numV? lval))
             (numV (+ (numV-n lval) (numV-n rval)))]
            [(and (numeqO? op) (numV? lval))
             (boolV (= (numV-n lval) (numV-n rval)))]
            [(and (appendO? op) (strV? lval))
             (strV (string-append (strV-s lval) (strV-s rval)))]
            [(and (streqO? op) (strV? lval))
             (boolV (string=? (strV-s lval) (strV-s rval)))]
            [else (error-bad-operands-simple)])
          (error-typecheck-binop-simple)))))                  

;; ENVIRONMENTS
(define-type Binding
  (bind [id : Symbol] [value : Value]))
(define-type-alias Environment (Listof Binding))
(define empty-env empty)
(define extend-env cons)

;; lookup : Symbol Environment -> Value
(define (lookup [name : Symbol] (env : Environment)) : Value
  (if (empty? env)
      (error-unbound-id-simple)
      (if (eq? name (bind-id (first env)))
          (bind-value (first env))
          (lookup name (rest env)))))

;; ExprS
(define-type ExprS
  (numS [n : Number])
  (strS [s : String])
  (idS [name : Symbol])
  (boolS [b : Boolean])
  (binopS [op : binops] [l : ExprS] [r : ExprS])
  (ifS [cond : ExprS] [conseq : ExprS] [alt : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (funS [param : Symbol] [body : ExprS])
  (appS [id : ExprS] [value : ExprS]))
                             
;; desugar : ExprS -> ExprC
(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(strS s) (strC s)]
    [(idS name) (idC name)]
    [(boolS b) (boolC b)]
    [(binopS op l r) (binopC op (desugar l) (desugar r))]
    [(ifS cond conseq alt) (ifC (desugar cond) (desugar conseq) (desugar alt))]
    [(funS param body) (funC param (desugar body))]
    [(appS procedure arg) (appC (desugar procedure) (desugar arg))]
    [(andS l r) (ifC (desugar l) (ifC (desugar r) (boolC #t) (boolC #f)) (boolC #f))]
    [(orS l r) (ifC (desugar l) (boolC #t) (ifC (desugar r) (boolC #t) (boolC #f)))]
    [(letS name value body) (appC (funC name (desugar body)) (desugar value))]))

(define (interp [e : ExprC])
  (interp-h e empty-env))

(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str))))

;; ERRORES
(define (write [l : (Listof String)])
  (if (empty? l)
      ""
      (string-append (first l)
                     (string-append " " (write (rest l))))))

(define (get-variant-value [v : Value]) : Symbol
  (cond
    [(numV? v) 'numV]
    [(boolV? v) 'boolV]
    [(strV? v) 'strV]
    [(funV? v) 'procV]))

(define (get-op-symbol [op : binops]) : Symbol
  (cond
    [(plusO? op) '+]
    [(numeqO? op) 'num=]
    [(appendO? op) '++]
    [(streqO? op) 'str=]))

(define (error-bad-conditional-simple)
  (error 'interp "no es un valor booleano"))

(define (error-bad-conditional [val : Symbol])
  (error 'interp-h
         (write (list
                 "bad conditional"
                 (symbol->string val)))))

(define (error-typecheck-binop-simple)
  (error 'interp "argumento incorrecto"))

(define (error-typecheck-binop [op : Symbol] [val1 : Symbol] [val2 : Symbol])
  (error 'interp-h
         (write (list
                 "type-check failed,"
                 (symbol->string val1)
                 "vs."
                 (symbol->string val2)
                 "in"
                 (symbol->string op)))))

(define (error-bad-operands-simple)
  (error 'interp "argumento incorrecto"))

(define (error-bad-operands [op : Symbol] [val1 : Symbol] [val2 : Symbol])
  (error 'interp-h
         (write (list
                 "bad operands in"
                 "("
                 (symbol->string op)
                 (symbol->string val1)
                 (symbol->string val2)
                 ")"))))
            
(define (error-unbound-id-simple)
  (error 'interp "identificador no está enlazado"))

(define (error-unbound-id [id : Symbol])
  (error 'interp-h
         (string-append
          "unbound identifier '"
          (symbol->string id))))

(define (error-bad-proc-simple)
  (error 'interp "no es una función"))
  
(define (error-bad-proc [name : String])
  (error 'interp-h
         (string-append
          name " not a function.")))

;; PARSE
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