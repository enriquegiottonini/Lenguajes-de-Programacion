#lang racket

#|

____ _  _ ___  _    _ ____ _ ___    ____ ____ ____ ____ 
|___  \/  |__] |    | |    |  |  __ |__/ |___ |___ [__  
|___ _/\_ |    |___ | |___ |  |     |  \ |___ |    ___] 
 

SINTAXIS CONCRETA
=================

Program := Expression
Expression := Number
Expression := -(Expression , Expression)
Expression := zero? (Expression)
Expression := if Expression then Expression else Expression
Expression := Identifier
Expression := let Identifier = Expression in Expression
Expression := proc (Identifier) Expression
Expression := (Expression Expression)
Expression := letrec Identifier (Identifier) = Expression in Expression
Expression := newref (Expression)
Expression := deref (Expression)
Expression := setref (Expression , Expression)

SINTAXIS ABSTRACTA
==================

Programa:
- (a-program exp1)

Expresión:
- (const-exp num)
- (diff-exp exp1 exp2)
- (zero?-exp exp1)
- (if-exp exp1 exp2 exp3)
- (var-exp var)
- (let-exp var exp1 body)
- (proc-exp var body)
- (call-exp rator rand)
- (letrec-exp p-name b-var p-body letrec-body)
- (newref-exp exp1)
- (deref-exp exp1)
- (setref-exp exp1 exp2)
- (list-exp exps)

|#

(struct a-program (exp1)
  #:transparent)

(define (program? x)
  (a-program? x))

(struct const-exp (num)
  #:transparent)

(struct diff-exp (exp1 exp2)
  #:transparent)

(struct zero?-exp (exp1)
  #:transparent)

(struct if-exp (exp1 exp2 exp3)
  #:transparent)

(struct var-exp (var)
  #:transparent)

(struct let-exp (var exp1 body)
  #:transparent)

(struct proc-exp (var body)
  #:transparent)

(struct call-exp (rator rand)
  #:transparent)

(struct letrec-exp (p-name b-var p-body letrec-body)
  #:transparent)

(struct newref-exp (exp1)
  #:transparent)

(struct deref-exp (exp1)
  #:transparent)

(struct setref-exp (exp1 exp2)
  #:transparent)
(struct list-exp (exps)
  #:transparent)


(define (expression? x)
  (or (const-exp? x)
      (diff-exp? x)
      (zero?-exp? x)
      (if-exp? x)
      (var-exp? x)
      (let-exp? x)
      (proc-exp? x)
      (call-exp? x)
      (letrec-exp? x)
      (newref-exp? x)
      (deref-exp? x)
      (setref-exp? x)
      (list-exp? x)))

;; parse : S-Expression -> program?
;; Traducción de S-Expressions a sintaxis abstracta de LET
(define (parse x)
  (a-program (parse-expression x)))

;; parse-expression : S-Expression -> expression?
(define (parse-expression x)
  (cond
    [(number? x) (const-exp x)]
    [(symbol? x) (var-exp x)]
    [(pair? x)
     (case (first x)
       [(emptylist) (emptylist null)] 
       [(-) (parse-diff x)]
       [(zero?) (parse-zero? x)]
       [(if) (parse-if x)]
       [(let) (parse-let x)]
       [(proc) (parse-proc x)]
       [(letrec) (parse-letrec x)]
       [(newref) (parse-newref x)]
       [(deref) (parse-deref x)]
       [(setref) (parse-setref x)]
       [(list) (parse-list x)]
       [else
        (if (= 2 (length x))
            (parse-call x)
            (error 'parse "expresión no es válida: ~e" x))])]
    [else
     (error 'parse "expresión no es válida: ~e" x)]))

;; parse-diff : pair? -> diff-exp?
;; x es un par de la forma (- . _)
(define (parse-diff x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (diff-exp (parse-expression (second x))
            (parse-expression (third x))))

;; parse-zero? : pair? -> zero?-exp?
;; x es un par de la forma (zero? . _)
(define (parse-zero? x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (zero?-exp (parse-expression (second x))))

;; parse-if : pair? -> if-exp?
;; x es un par de la forma (if . _)
(define (parse-if x)
  (unless (= (length x) 4)
    (error 'parse "expresión no es válida: ~e" x))
  (if-exp (parse-expression (second x))
          (parse-expression (third x))
          (parse-expression (fourth x))))

;; parse-let : pair? -> let-exp?
;; x es un par de la forma (let . _)
(define (parse-let x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (let ([binding (second x)])
    (unless (= (length binding) 2)
      (error 'parse "expresión no es válida: ~e" x))
    (unless (symbol? (first binding))
      (error 'parse "expresión no es válida: ~e" x))
    (let-exp (first binding)
             (parse-expression (second binding))
             (parse-expression (third x)))))

;; parse-proc : pair? -> proc-exp?
;; x es un par de la forma (proc . _)
(define (parse-proc x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (unless (symbol? (second x))
    (error 'parse "expresión no es válida: ~e" x))
  (proc-exp (second x)
            (parse-expression (third x))))

;; parse-call : list? -> call-exp?
;; x es una lista de dos elementos
(define (parse-call x)
  (call-exp (parse-expression (first x))
            (parse-expression (second x))))

;; parse-letrec : pair? -> letrec-exp?
;; x es un par de la forma (letrec . _)
(define (parse-letrec x)
  (unless (= (length x) 5)
    (error 'parse "expresión no es válida: ~e" x))
  (let ([p-name (second x)]
        [b-var (third x)])
    (unless (= (length b-var) 1)
      (error 'parse "expresión no es válida: ~e" x))
    (unless (and (symbol? p-name) (symbol? (first b-var)))
      (error 'parse "expresión no es válida: ~e" x))
    (letrec-exp p-name
                (first b-var)
                (parse-expression (fourth x))
                (parse-expression (fifth x)))))

;; parse-newref : pair? -> newref-exp
;; x es un par de la forma (newref . _)
(define (parse-newref x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (newref-exp (parse-expression (second x))))

;; parse-deref : pair? -> deref-exp
;; x es un par de la forma (deref . _)
(define (parse-deref x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (deref-exp (parse-expression (second x))))

;; parse-setref : pair? -> setref-exp
;; x es un par de la forma (setref . _)
(define (parse-setref x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (setref-exp (parse-expression (second x)) (parse-expression (third x))))
;; parse-begin : pair? -> list-exp
;; x es un par de la forma (list . _)
(define (parse-list x)
  (unless (= (length x) 2)
    (error 'parse "expresion no es válida: ~e" x))
  (list-exp (parse-expressions (second x))))

(define (parse-expressions x)
  (if (null? x)
      null
  (cons (parse-expression (first x)) (parse-expressions (rest x)))))
 
;;;;;;;;;;;;;;
;; ENTORNOS ;;
;;;;;;;;;;;;;;

(define (empty-env)
  null)

(define (apply-env env var)
  (cond [(null? env)
         (error 'environment "variable libre: ~e" var)]
        [(equal? var (car (car env)))
         (if (vector? (second (first env)))
             (vector-ref (second (first env)) 0)
             (second (first env)))]
        [else
         (apply-env (cdr env) var)]))

(define (extend-env var val env)
  (cons (list var val) env))

(define (extend-env-rec p-name b-var body saved-env)
  (let ((vec (make-vector 1)))
    (let ((new-env (extend-env p-name vec saved-env)))
      (vector-set! vec 0
                   (proc-val (procedure b-var body new-env)))
      new-env)))

;;;;;;;;;;;;;;;;;;;;
;; PROCEDIMIENTOS ;;
;;;;;;;;;;;;;;;;;;;;

(struct procedure (var body saved-env)
  #:transparent)

(define (apply-procedure proc val s)
  (unless (procedure? proc)
    (error 'value-of "no es un procedimiento: ~e" proc))
  (let ([var (procedure-var proc)]
        [body (procedure-body proc)]
        [saved-env (procedure-saved-env proc)])
    (value-of body (extend-env var val saved-env) s)))

;;;;;;;;;;;;
;; STORE  ;;
;;;;;;;;;;;;

;; empty-store : () -> Sto
(define (empty-store)
  null)

(define the-store 'dummy-init)

;; get-store : () -> Sto
(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

;; reference : RacketVal -> Bool
(define (reference? v)
  (integer? v))

;;;;;;;;;;;;;;;;;;
;; COMPUTATION  ;;
;;;;;;;;;;;;;;;;;;
(struct computation (val store)
  #:transparent)



;;;;;;;;;;;;;;;
;; SEMÁNTICA ;;
;;;;;;;;;;;;;;;

#|

VALORES EXPRESADOS Y DENOTADOS

ExpVal = Int + Bool + Proc + Ref(ExpVal)* + Null + EmptyList + PairVal
DenVal = ExpVal

*Here Ref(ExpVal) means the set of references to locations that contain
expressed values.
|#

(struct num-val (num)
  #:transparent
  #:guard (lambda (num type-name)
            (unless (number? num)
              (error type-name "no es un número: ~e" num))
            num))

(define expval->num
  (lambda (ev)
    (cond
      [(num-val? ev) (num-val-num ev)]
      [(ref-val? ev) (ref-val-ref ev)])))

(struct bool-val (bool)
  #:transparent
  #:guard (lambda (bool type-name)
            (unless (boolean? bool)
              (error type-name "no es un booleano: ~e" bool))
            bool))

(define expval->bool bool-val-bool)

(struct proc-val (proc)
  #:transparent
  #:guard (lambda (proc type-name)
            (unless (procedure? proc)
              (error type-name "no es un procedimiento: ~e" proc))
            proc))

(define expval->proc proc-val-proc)

(struct ref-val (ref)
  #:transparent
  #:guard (lambda (ref type-name)
            (unless (reference? ref)
              (error type-name "no es una referencia ~e" ref))
            ref))

(define expval->ref ref-val-ref)

(struct pair-val (exp)
  #:transparent)

(struct null-val (null)
  #:transparent)

(struct emptylist (null)
  #:transparent)
#|

ESPECIFICACIONES SEMÁNTICAS
------------------------------------------------------------
(value-of (const-exp n) env s) = ((num-val n), s)
------------------------------------------------------------
(value-of (var-exp var) env s) = ((apply-env env var) s)
------------------------------------------------------------
(value-of (diff-exp exp1 exp2) env s) = (diffval, s2)
where:
(val1, s1) = (value-of exp1 env s)
(val2, s2) = (value-of exp2 env s1)
diffval = (num-val
          (- (expval->num val1)
             (expval->num val2)))
------------------------------------------------------------
(value-of (zero?-exp exp1) env s)
 = (if (equal? 0 (expval->num val1))
       (bool-val #t s1)
       (bool-val #f s1))
where:
(val1, s1) = (value-of exp1 env s)
------------------------------------------------------------
(value-of (if-exp exp1 exp2 exp3) env s)
 = (if (expval->bool val1)
       (value-of exp2 env)
       (value-of exp3 env))
where:
(val1 s1) = (value-of exp1 env s)
------------------------------------------------------------
(value-of (let-exp var exp1 body) env)
 = (value-of body (extend-env var val1 env) s1)
where:
(val1, s1) = (value-of exp1 env s)
------------------------------------------------------------
(value-of (proc-exp var body) env s)
 = (proc-val (procedure var body env))
------------------------------------------------------------
(value-of (call-exp rator rand) env s)
 = (let ([proc (expval->proc val1)]
         [arg val2])
     (apply-procedure proc arg))
where:
(val1, s1) = (value-of rator env s)
(val2, s2) = (value-of rand env s1)
------------------------------------------------------------
(value-of (newref-exp exp1) env s) = ((ref-val loc), [loc=val]s1)
where:
(value-of exp1 env s) = (val, s1) and
l \not \in dom(s1)
------------------------------------------------------------
(value-of (deref-exp exp1) env s) = (s1(loc), s1)
where:
(value-of exp env s) = (l, s1)
------------------------------------------------------------
(value-of (setref-exp exp1 exp2) env s) = (val, [loc=val]s2)
where:
(value-of exp1 env s)  = (loc, s1)
(value-of exp2 env s1) = (val, s2)
------------------------------------------------------------
|#

(define (value-of exp env s)
  (cond
    [(const-exp? exp)
     (let ([n (const-exp-num exp)])
       (computation (num-val n) s))]
    [(var-exp? exp)
     (let ([var (var-exp-var exp)])
       (computation (apply-env env var) s))]
    [(diff-exp? exp)
     (let* ([exp1 (diff-exp-exp1 exp)]
            [exp2 (diff-exp-exp2 exp)]
            [comp1 (value-of exp1 env s)]
            [val1 (computation-val comp1)]
            [store1 (computation-store comp1)]
            [comp2 (value-of exp2 env store1)]
            [val2 (computation-val comp2)]
            [store2 (computation-store comp2)])
       (computation (num-val
                     (- (expval->num val1)
                        (expval->num val2))) store2))]
    [(zero?-exp? exp)
     (let* ([exp1 (zero?-exp-exp1 exp)]
            [comp1 (value-of exp1 env s)]
            [val1 (computation-val comp1)]
            [store1 (computation-store comp1)])
       (if (equal? 0 (expval->num val1))
           (computation (bool-val #t) store1)
           (computation (bool-val #f) store1)))]
    [(if-exp? exp)
     (let* ([exp1 (if-exp-exp1 exp)]
            [exp2 (if-exp-exp2 exp)]
            [exp3 (if-exp-exp3 exp)]
            [comp1 (value-of exp1 env s)]
            [val1 (computation-val comp1)]
            [store1 (computation-store comp1)])
       (if (expval->bool val1)
           (value-of exp2 env store1)
           (value-of exp3 env store1)))]
    [(let-exp? exp)
     (let* ([var (let-exp-var exp)]
            [exp1 (let-exp-exp1 exp)]
            [body (let-exp-body exp)]
            [comp1 (value-of exp1 env s)]
            [val1 (computation-val comp1)]
            [store1 (computation-store comp1)])
       (value-of body (extend-env var val1 env) store1))]
    [(proc-exp? exp)
     (let ([var (proc-exp-var exp)]
           [body (proc-exp-body exp)])
       (computation (proc-val (procedure var body env))
                    s))]
    [(call-exp? exp)
     (let ([rator (call-exp-rator exp)]
           [rand (call-exp-rand exp)])
       (let* ([comp1 (value-of rator env s)]
              [vrator (computation-val comp1)]
              [store1 (computation-store comp1)]
              [proc (expval->proc vrator)]
              [comp2 (value-of rand env store1)]
              [varg (computation-val comp2)]
              [store2 (computation-store comp2)]
              [arg varg])
         (apply-procedure proc arg store2)))]
    [(letrec-exp? exp)
     (let ([p-name (letrec-exp-p-body exp)]
           [b-var (letrec-exp-b-var exp)]
           [p-body (letrec-exp-p-body exp)]
           [letrec-body (letrec-exp-letrec-body exp)])
       (value-of letrec-body (extend-env-rec p-name b-var p-body env) s))]
    [(newref-exp? exp)
     (let* ([exp1 (newref-exp-exp1 exp)]
            [next-ref (length the-store)]
            [cmp1 (value-of exp1 env s) ]
            [val1 (computation-val cmp1)]
            [s1 (computation-store cmp1)])
       (set! the-store (append the-store (list val1)))
       (computation (ref-val next-ref) the-store))]
    [(deref-exp? exp)
     (let* ([exp1 (deref-exp-exp1 exp)]
            [cmp1 (value-of exp1 env s)]
            [val1 (computation-val cmp1)]
            [s1 (computation-store cmp1)]
            [loc (expval->num val1)])
       (if (or
            (empty? (get-store))
            (< loc 0)
            (<= (length the-store) loc))
           (error 'deref-exp "no se puede encontrar la locación ~e" loc)
           (computation (list-ref the-store loc) s1)))]
    [(setref-exp? exp)
     (let* ([exp1 (setref-exp-exp1 exp)]
            [exp2 (setref-exp-exp2 exp)]
            [cmp1 (value-of exp1 env s)]
            [val1 (computation-val cmp1)]
            [s1 (computation-store cmp1)]
            [cmp2 (value-of exp2 env s1)]
            [val2 (computation-val cmp2)]
            [s2 (computation-store cmp2)]
            [loc (expval->num val1)])
             (if (or
                  (empty? (get-store))
                  (< loc 0)
                  (<= (length the-store) loc))
                 (error 'deref-exp "no se puede encontrar la locación ~e" loc)
                 (begin
                   (set! the-store (list-set the-store loc val2))
                   (computation val2 the-store))))]
    [(list-exp? exp)
     (let* ([exps (list-exp-exps exp)])
       (if (null? exps)
            (null-val null)
           (let* ([val1 (value-of (first exps) env s)]
                  [s1 (computation-store val1)])
             (pair-val (cons val1 (value-of (list-exp (rest exps)) env s1))))))]
    [(emptylist? exp)
     (computation (emptylist-null exp) the-store)]
[else
 ((error 'value-of "no es una expresión: ~e" exp))]))

(define (init-env)
  (foldl (lambda (binding env)
           (extend-env (first binding) (second binding) env))
         (empty-env)
         (list (list 'π (num-val 3.141592653589793))
               (list 'e (num-val 2.718281828459045))
               (list 'i (num-val 0+1i))
               (list 'G (num-val 6.674e-11))
               (list 'c (num-val 299792458))
               (list 'h (num-val 6.62607015e-34)))))

(define (value-of-program pgm)
  (if (program? pgm)
      (let ([exp1 (a-program-exp1 pgm)])
        (initialize-store!)
        (value-of exp1 (init-env) (get-store)))
      (error 'value-of-program "no es un programa: ~e" pgm)))

(define (run sexp)
  (value-of-program (parse sexp)))

(provide (all-defined-out))
