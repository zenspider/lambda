#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide #%module-begin
         #%datum
         prog expr func appl name body lexp aexp)

(define-syntax-rule (prog e ...)         (begin e ...))
(define-syntax-rule (expr e)             e)
(define-syntax-rule (func _ name _ body) (list 'lambda (list name) body))
(define-syntax-rule (appl _ lexp aexp _) (list lexp aexp))
(define-syntax-rule (name id)            'id)
(define-syntax-rule (body e)             e)
(define-syntax-rule (lexp e)             e)
(define-syntax-rule (aexp e)             e)
