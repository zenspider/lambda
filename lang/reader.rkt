#lang s-exp syntax/module-reader
lambda/semantics
#:read my-read
#:read-syntax my-read-syntax
#:whole-body-readers? #t

(require "../lexer.rkt"
         "../parser.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (list (parse src (tokenize ip))))
