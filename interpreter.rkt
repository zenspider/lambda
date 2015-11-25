#lang racket/base

(require syntax/parse)

;; (define (interpret-drawing drawing-stx)
;;   (syntax-parse drawing-stx
;;     [({~literal drawing} rows-stxs ...)
;;
;;      (for ([rows-stx (syntax->list #'(rows-stxs ...))])
;;        (interpret-rows rows-stx))]))
;;
;; (define (interpret-rows rows-stx)
;;   (syntax-parse rows-stx
;;     [({~literal rows}
;;       ({~literal repeat} repeat-number)
;;       chunks ... ";")
;;
;;      (for ([i (syntax-e #'repeat-number)])
;;        (for ([chunk-stx (syntax->list #'(chunks ...))])
;;          (interpret-chunk chunk-stx))
;;        (newline))]))
;;
;; (define (interpret-chunk chunk-stx)
;;   (syntax-parse chunk-stx
;;     [({~literal chunk} chunk-size chunk-string)
;;
;;      (for ([k (syntax-e #'chunk-size)])
;;        (display (syntax-e #'chunk-string)))]))
;;
;; (module+ test
;;   (require rackunit)
;;   (require racket/port)
;;   (require racket/function)
;;   (require "parser.rkt")
;;   (require "lexer.rkt")
;;
;;   (check-equal? (with-output-to-string
;;                   (thunk (interpret-drawing (parse (tokenize (open-input-string "3 9 X; 6 3 b 3 X 3 b; 3 9 X;"))))))
;;                 "XXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\n   XXX   \n   XXX   \n   XXX   \n   XXX   \n   XXX   \n   XXX   \nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\n"
;;                 )
;;
;;   )
