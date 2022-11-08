;;; -*- mode: racket; coding: utf-8 -*-
;;; 
;;; `7MMF'      `7MM"""YMM  `YMM'   `MP' 
;;;   MM          MM    `7    VMb.  ,P   
;;;   MM          MM   d       `MM.M'    
;;;   MM          MMmmMM         MMb     
;;;   MM      ,   MM   Y  ,    ,M'`Mb.   
;;;   MM     ,M   MM     ,M   ,P   `MM.  
;;; .JMMmmmmMMM .JMMmmmmMMM .MM:.  .:MMa.
;;; 
;;; Lexical Analyzer for letrec language

#lang racket/base

(require "lex-gen.rkt")

(module+ test
  (require rackunit
           rackunit/text-ui)

  (port-count-lines-enabled #t))

(struct token (name value beg end)
  #:transparent)

(define reg-nat
  (reg-repeat 1 +inf.0 (char-set "0123456789")))

(define reg-float
  (reg-conc reg-nat "." reg-nat))

(define reg-almost-float
  (reg-union (reg-conc "." reg-nat)
             (reg-conc reg-nat ".")))

(define (lex-number)
  (lambda (src lexeme beg end)
    (token 'number (string->number lexeme) beg end)))

(define (lex-almost-float src lexeme beg end)
  (error 'letrec-parse
         (string-append
          "malformed input at line ~v column ~v: unexpected ~v\n"
          "  maybe you forgot a digit before or after the dot?")
         (pos-line beg)
         (pos-col beg)
         lexeme))

(define reg-alphabetic
  (reg-repeat 1 +inf.0 just-alphabetic))

(define reg-id
  (reg-conc reg-alphabetic
            (reg-kleene reg-nat)))

(define reg-char-and-digits
  (reg-conc reg-id
            (reg-kleene reg-id)))
  
(define reg-almost-id
  (reg-union
   reg-char-and-digits
   (reg-conc reg-alphabetic reg-char-and-digits)))

(define (lex-almost-id src lexeme beg end)
  (error 'letrec-parse
         (string-append
          "malformed input at line ~v column ~v: unexpected ~v\n"
          "  remember that identifiers are characters followed by 0 or more digits only")
         (pos-line beg)
         (pos-col beg)
         lexeme))

(define (lex-identifier)
  (lambda (src lexeme beg end)
    (token 'identifier (string->symbol lexeme) beg end)))

(define reg-space
  (reg-repeat 1 +inf.0 just-whitespace))

(define (lex-space src lexeme beg end)
  (lex-letrec src))

(define (lex-terminal name)
  (lambda (src lexeme beg end)
    (token name #f beg end)))

(define reg-delims
  (char-set-comp "(" ")"
                 #\space #\newline #\return #\tab
                 "-" "="))

(define reg-kw-delim
  (reg-conc (reg-union "zero?" "if" "then"
                       "else" "let" "in"
                       "proc" "letrec")
            reg-delims))   

(define (lex-kw-delim src lexeme beg end)
  (error (format "Se esperaba un delimitador después del ~a"
                 (substring lexeme 0 (sub1 (string-length lexeme))))
         beg end))

(define reg-nat-delim
  (reg-conc reg-nat reg-delims))

(define (lex-nat-delim src lexeme beg end)
  (error (format "Se esperaba un delimitador después de un número")
         beg end))

(define reg-id-delim
  (reg-conc reg-id reg-delims))

(define (lex-id-delim src lexeme beg end)
  (error (format "Se esperaba un delimitador después de una variable")
         beg end))

(define lex-letrec
  (make-lexer
   'letrec
   (lex-rule reg-nat (lex-number))
   (lex-rule reg-float (lex-number))
   (lex-rule reg-space lex-space)
   (lex-rule "(" (lex-terminal 'oparen))
   (lex-rule ")" (lex-terminal 'cparen))
   (lex-rule "-" (lex-terminal '-))
   (lex-rule "=" (lex-terminal 'equal))
   (lex-rule "zero?" (lex-terminal 'zero-predicate))
   (lex-rule "if" (lex-terminal 'if))
   (lex-rule "then" (lex-terminal 'then))
   (lex-rule "else" (lex-terminal 'else))
   (lex-rule "let" (lex-terminal 'let))
   (lex-rule "in" (lex-terminal 'in))
   (lex-rule "proc" (lex-terminal 'proc))
   (lex-rule "letrec" (lex-terminal 'letrec))
   (lex-rule reg-kw-delim lex-kw-delim)
   (lex-rule reg-nat-delim lex-nat-delim)
   (lex-rule reg-id (lex-identifier))
   ;; common errors
   (lex-rule reg-almost-float lex-almost-float)
   (lex-rule reg-almost-id lex-almost-id)))

(define (lex-letrec* src)
  (define t (lex-letrec src))
  (if (eof-object? t)
      null
      (cons t (lex-letrec* src))))

(define (lex str)
  (map (lambda (t)
         (list (token-name t) (token-value t)))
       (lex-letrec* (open-input-string str))))

(module+ test
  (run-tests
   (test-suite
    "letrec lex test"
    (check-equal? (lex "") '())
    (check-equal? (lex "0") '((number  0)))
    (check-equal? (lex "000") '((number 0)))
    (check-equal? (lex "025") '((number 25)))
    (check-equal? (lex "86420") '((number 86420)))
    (check-equal? (lex "0.5") '((number 0.5)))
    (check-equal? (lex "foo") '((identifier foo)))
    (check-equal? (lex "foo231") '((identifier foo231)))
    (check-equal? (lex "1   foo 3 foo2   ")
                  '((number 1)
                    (identifier foo)
                    (number 3)
                    (identifier foo2)))
    (check-equal? (lex "let x = 200
                        in let f = proc (z) (- z 100)
                           in (f x)")
                  '((let #f)
                    (identifier x)
                    (equal #f)
                    (number 200)
                    (in #f)
                    (let #f)
                    (identifier f)
                    (equal #f)
                    (proc #f)
                    (oparen #f)
                    (identifier z)
                    (cparen #f)
                    (oparen #f)
                    (- #f)
                    (identifier z)
                    (number 100)
                    (cparen #f)
                    (in #f)
                    (oparen #f)
                    (identifier f)
                    (identifier x)
                    (cparen #f)))
    (check-equal? (lex "letrec double(x) = if zero?(x)
                                           then 0
                                           else -((double -(x 1)) -2)
                        in (double 6)")
                  '((letrec #f)
                    (identifier double)
                    (oparen #f)
                    (identifier x)
                    (cparen #f)
                    (equal #f)
                    (if #f)
                    (zero-predicate #f)
                    (oparen #f)
                    (identifier x)
                    (cparen #f)
                    (then #f)
                    (number 0)
                    (else #f)
                    (- #f)
                    (oparen #f)
                    (oparen #f)
                    (identifier double)
                    (- #f)
                    (oparen #f)
                    (identifier x)
                    (number 1)
                    (cparen #f)
                    (cparen #f)
                    (- #f)
                    (number 2)
                    (cparen #f)
                    (in #f)
                    (oparen #f)
                    (identifier double)
                    (number 6)
                    (cparen #f)))
    (check-exn exn:fail?
               (lambda () (lex ".5")))
    (check-exn exn:fail?
               (lambda () (lex "1.")))
    (check-exn exn:fail?
               (lambda () (lex "-(1 .5)")))
    (check-exn exn:fail?
               (lambda () (lex "letx = 1 in -(x 1)")))
    (check-exn exn:fail?
               (lambda () (lex "let 3b = 1 in -(x 1)")))
    (check-exn exn:fail?
               (lambda () (lex "let b3b3 = 1 in -(x 1)"))))))