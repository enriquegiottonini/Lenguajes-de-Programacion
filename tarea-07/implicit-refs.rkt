#lang racket

#|

 (                                  (                 
 )\ )             (             )   )\ )    (         
(()/(   )         )\(     (  ( /(  (()/(  ( )\ )      
 /(_)) (    `  ) ((_)\  ( )\ )\())  /(_))))(()/( (    
(_))   )\  '/(/(  _((_) )((_|_))/  (_)) /((_)(_)))\   
|_ _|_((_))((_)_\| |(_)((_|_) |_   | _ (_))(_) _((_)  
 | || '  \() '_ \) || / _|| |  _|  |   / -_)|  _(_-<  
|___|_|_|_|| .__/|_||_\__||_|\__|  |_|_\___||_| /__/  
           |_|                                        

A simple statement oriented language with implicit references.

SINTAXIS CONCRETA
=================

Program    := Statement
Statement  := Identifier = Expression
           := print Expression
           := {{Statement}*(;) }
           := if Expression Statement Statement
           := while Expression Statement
           := var {Identifier}*(,)} ; Statement

Expression := Number
           := -(Expression , Expression)
           := zero? (Expression)
           := if Expression then Expression else Expression
           := Identifier
           := let Identifier = Expression in Expression
           := proc (Identifier) Expression
           := (Expression Expression)
           := set Identifier = Expression

SINTAXIS ABSTRACTA
=================

Program:
- (a-program stmt)

Statement:
- (assign-stmt var exp1)
- (print-stmt exp1)
- (pair-stmt stmt1 stmts)
- (cond-stmt exp1 stmt1 stmt2)
- (loop-stmt exp1 stmt1)
- (declr-stmt vars stmt1)

Expression:
- (const-exp num)
- (diff-exp exp1 exp2)
- (zero?-exp exp1)
- (if-exp exp1 exp2 exp3)
- (var-exp var)
- (let-exp var exp1 body)
- (proc-exp var body)
- (call-exp rator rand)
- (assign-exp (var exp1)

|#

;; Program
(struct a-program (stmt)
  #:transparent)

;; Statement
(struct assign-stmt (var exp1)
  #:transparent)

(struct print-stmt (exp1)
  #:transparent)

(struct pair-stmt (stmt1 stmts)
  #:transparent)

(struct cond-stmt (exp1 stmt1 stmt2)
  #:transparent)

(struct loop-stmt (exp1 stmt1)
  #:transparent)

(struct declr-stmt (vars stmt1)
  #:transparent)

;; Expression
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

(struct newref-exp (exp1)
  #:transparent)

(struct deref-exp (exp1)
  #:transparent)

(struct setref-exp (exp1 exp2)
  #:transparent)

(struct assign-exp (var exp1)
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
      (newref-exp? x)
      (deref-exp? x)
      (setref-exp? x)
      (assign-exp? x)))

#|                                                                                
 _____         _                           _       
|   __|___ _ _|_|___ ___ ___ _____ ___ ___| |_ ___ 
|   __|   | | | |  _| . |   |     | -_|   |  _|_ -|
|_____|_|_|\_/|_|_| |___|_|_|_|_|_|___|_|_|_| |___|
                                                  
|#

(define (empty-env)
  null)

(define (apply-env env var)
  (cond [(null? env)
         (error 'environment "unbounded variable: ~e" var)]
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

#|
 _____                   _                 
|  _  |___ ___ ___ ___ _| |_ _ ___ ___ ___ 
|   __|  _| . |  _| -_| . | | |  _| -_|_ -|
|__|  |_| |___|___|___|___|___|_| |___|___|
                                           
|#

(struct procedure (var body saved-env)
  #:transparent)

(define (apply-procedure proc val s)
  (unless (procedure? proc)
    (error 'value-of "not a procedure: ~e" proc))
  (let* ([var (procedure-var proc)]
         [body (procedure-body proc)]
         [saved-env (procedure-saved-env proc)]
         [loc (length s)])
    (value-of body
              (extend-env var loc saved-env)
              (append s (list val)))))

#|                         
 _____ _                   
|   __| |_ ___ ___ ___ ___ 
|__   |  _| . |  _| -_|_ -|
|_____|_| |___|_| |___|___|
                                                       
|#

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

#|                                   
 _____                   _   _         
|   __|___ _____ ___ ___| |_|_|___ ___ 
|__   | -_|     | .'|   |  _| |  _| .'|
|_____|___|_|_|_|__,|_|_|_| |_|___|__,|


VALORES EXPRESADOS Y DENOTADOS

ExpVal = Int+Bool+Proc
DenVal = Ref(ExpVal)

|#

(struct an-answer (val store)
  #:transparent)

(struct num-val (num)
  #:transparent
  #:guard (lambda (num type-name)
            (unless (number? num)
              (error type-name "not a number: ~e" num))
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
              (error type-name "not a boolean: ~e" bool))
            bool))

(define expval->bool bool-val-bool)

(struct proc-val (proc)
  #:transparent
  #:guard (lambda (proc type-name)
            (unless (procedure? proc)
              (error type-name "not a procedure: ~e" proc))
            proc))

(define expval->proc proc-val-proc)

(struct ref-val (ref)
  #:transparent
  #:guard (lambda (ref type-name)
            (unless (reference? ref)
              (error type-name "not a location ~e" ref))
            ref))

(define expval->ref ref-val-ref)

#|

ESPECIFICACIONES SEMÁNTICAS

(value-of (const-exp n) env s) = ((num-val n), s)
 
(value-of (var-exp var) env s) = (s(env(var)), s)
 
(value-of (diff-exp exp1 exp2) env s) = (diffval, s2)
where:
(val1, s1) = (value-of exp1 env s)
(val2, s2) = (value-of exp2 env s1)
diffval = (num-val
          (- (expval->num val1)
             (expval->num val2)))
 
(value-of (zero?-exp exp1) env s)
 = (if (equal? 0 (expval->num val1))
       (bool-val #t s1)
       (bool-val #f s1))
where:
(val1, s1) = (value-of exp1 env s)
 
(value-of (if-exp exp1 exp2 exp3) env s)
 = (if (expval->bool val1)
       (value-of exp2 env)
       (value-of exp3 env))
where:
(val1 s1) = (value-of exp1 env s)
 
(value-of (let-exp var exp1 body) env)
 = (value-of body (extend-env var loc env) [loc=val1]s1)
where:
(val1, s1) = (value-of exp1 env s)
 
(value-of (proc-exp var body) env s)
 = (proc-val (procedure var body env))
 
(value-of (call-exp rator rand) env s)
 = (let ([proc (expval->proc val1)]
         [arg val2])
     (apply-procedure proc arg))
where:
(val1, s1) = (value-of rator env s)
(val2, s2) = (value-of rand env s1)

(value-of (newref-exp exp1) env s) = ((ref-val loc), [loc=val]s1)
where:
(value-of exp1 env s) = (val, s1) and
l \not \in dom(s1)

(value-of (deref-exp exp1) env s) = (s1(loc), s1)
where:
(value-of exp env s) = (l, s1)

(value-of (setref-exp exp1 exp2) env s) = (val, [loc=val]s2)
where:
(value-of exp1 env s)  = (loc, s1)
(value-of exp2 env s1) = (val, s2)

(value-of (assign-exp var exp1) env s) = (val1 ,[env(var) = val1]s1)
where:
(value-of (exp1 env s) = (val1, s1)

|#

(define (value-of exp env s)
  (cond
    [(const-exp? exp)
     (let ([n (const-exp-num exp)])
       (an-answer (num-val n) s))]
    [(var-exp? exp)
     (let ([var (var-exp-var exp)])
       (an-answer (list-ref s (apply-env env var)) s))]
    [(diff-exp? exp)
     (let* ([exp1 (diff-exp-exp1 exp)]
            [exp2 (diff-exp-exp2 exp)]
            [asw1 (value-of exp1 env s)]
            [val1 (an-answer-val asw1)]
            [store1 (an-answer-store asw1)]
            [asw2 (value-of exp2 env store1)]
            [val2 (an-answer-val asw2)]
            [store2 (an-answer-store asw2)])
       (an-answer (num-val
                   (- (expval->num val1)
                      (expval->num val2))) store2))]
    [(zero?-exp? exp)
     (let* ([exp1 (zero?-exp-exp1 exp)]
            [asw1 (value-of exp1 env s)]
            [val1 (an-answer-val asw1)]
            [store1 (an-answer-store asw1)])
       (if (equal? 0 (expval->num val1))
           (an-answer (bool-val #t) store1)
           (an-answer (bool-val #f) store1)))]
    [(if-exp? exp)
     (let* ([exp1 (if-exp-exp1 exp)]
            [exp2 (if-exp-exp2 exp)]
            [exp3 (if-exp-exp3 exp)]
            [asw1 (value-of exp1 env s)]
            [val1 (an-answer-val asw1)]
            [store1 (an-answer-store asw1)])
       (if (expval->bool val1)
           (value-of exp2 env store1)
           (value-of exp3 env store1)))]
    [(let-exp? exp)
     (let* ([var  (let-exp-var exp)]
            [exp1 (let-exp-exp1 exp)]
            [body (let-exp-body exp)]
            [asw1 (value-of exp1 env s)]
            [val1 (an-answer-val asw1)]
            [s1 (an-answer-store asw1)])
       (value-of body (extend-env var (length s1) env) (append s1 (list val1))))]
    [(proc-exp? exp)
     (let ([var (proc-exp-var exp)]
           [body (proc-exp-body exp)])
       (an-answer (proc-val (procedure var body env))
                  s))]
    [(call-exp? exp)
     (let ([rator (call-exp-rator exp)]
           [rand (call-exp-rand exp)])
       (let* ([asw1 (value-of rator env s)]
              [vrator (an-answer-val asw1)]
              [store1 (an-answer-store asw1)]
              [proc (expval->proc vrator)]
              [asw2 (value-of rand env store1)]
              [varg (an-answer-val asw2)]
              [store2 (an-answer-store asw2)]
              [arg varg])
         (apply-procedure proc arg store2)))]
    [(newref-exp? exp)
     (let* ([exp1 (newref-exp-exp1 exp)]
            [next-ref (length s)]
            [asw1 (value-of exp1 env s) ]
            [val1 (an-answer-val asw1)]
            [s1   (an-answer-store asw1)])
       (an-answer (ref-val next-ref) (append s (list val1))))]
    [(deref-exp? exp)
     (let* ([exp1 (deref-exp-exp1 exp)]
            [asw1 (value-of exp1 env s)]
            [val1 (an-answer-val asw1)]
            [s1   (an-answer-store asw1)]
            [loc  (expval->num val1)])
       (if (or (empty? s1)
               (> 0 loc)
               (<= (length s1) loc))
           (error 'deref-exp "location not found: ~e" loc)
           (an-answer (list-ref s1 loc) s1)))]
    [(setref-exp? exp)
     (let* ([exp1 (setref-exp-exp1 exp)]
            [exp2 (setref-exp-exp2 exp)]
            [asw1 (value-of exp1 env s)]
            [val1 (an-answer-val asw1)]
            [s1   (an-answer-store asw1)]
            [asw2 (value-of exp2 env s1)]
            [val2 (an-answer-val asw2)]
            [s2   (an-answer-store asw2)]
            [loc  (expval->num val1)])
       (if (or (empty? s2)
               (< 0 loc)
               (<= (length s2) loc))
           (error 'setref-exp "location not found: ~e" loc)
           (an-answer val2 (list-set s2 loc val2))))]
    [(assign-exp? exp)
     (let* ([var  (assign-exp-var exp)]
            [exp1 (assign-exp-exp1 exp)]
            [aws1 (value-of exp1 env s)]
            [val1 (an-answer-val aws1)]
            [s1   (an-answer-store aws1)])
       (an-answer val1 (list-set s1 (apply-env env var) val1)))]
    [else
     ((error 'value-of "no es una expresión: ~e" exp))]))

#|
                                                               
  *   )             )                   
` )  /(   (      ( /( (          (  (   
 ( )(_)) ))\ (   )\()))\   (     )\))(  
(_(_()) /((_))\ (_))/((_)  )\ ) ((_))\  
|_   _|(_)) ((_)| |_  (_) _(_/(  (()(_) 
  | |  / -_)(_-<|  _| | || ' \))/ _` |  
  |_|  \___|/__/ \__| |_||_||_| \__, |  
                                |___/   
                            
|#

(require rackunit
         rackunit/text-ui)

(initialize-store!)
(define s the-store)
(define e (empty-env))
(define m (let-exp 'x (const-exp 3)
                     (let-exp 'f (proc-exp 'y
                                           (assign-exp 'x (var-exp 'y)))
                              (call-exp (var-exp 'f) (const-exp 10)))))