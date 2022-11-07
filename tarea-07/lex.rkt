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

(define (lex-identifier)
  (lambda (src lexeme beg end)
    (token 'identifier (string->symbol lexeme) beg end)))

(define lex-letrec
  (make-lexer
   'letrec
   (lex-rule reg-nat (lex-number))
   (lex-rule reg-float (lex-number))
   (lex-rule reg-id (lex-identifier))
   ;; common errors
   (lex-rule reg-almost-float lex-almost-float)
   ))

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
    
    )))