#lang racket/base

(provide tokenize)

(require parser-tools/lex)
(require ragg/support)
(require (prefix-in : parser-tools/lex-sre))
(require racket/function)

;; (define-tokens content-tokens
;;   (NAME ERROR))
;;
;; (define-empty-tokens delim-tokens
;;   (EOF LAMBDA OPEN CLOSE DOT WHITESPACE))

(define-lex-abbrevs                     ; stolen from ragg
  [letter  (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit   (:/ #\0 #\9)]
  [id-char (:or letter digit (char-set "-!$%&/<=>?^_~@"))])

(define-lex-abbrev id
  (:& (complement (:+ digit))
      (:+ id-char)))

(define (tokenize ip)
  (port-count-lines! ip)
  (define lexer
    (lexer-src-pos
     [id              (token 'NAME (string->symbol lexeme))]
     ["λ"             (token 'LAMBDA)]
     ["("             (token 'OPEN)]
     [")"             (token 'CLOSE)]
     ["."             (token 'DOT)]
     [(:+ whitespace) (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof)           (void)]
     ))
  (thunk (lexer ip)))

;; (define (tokenize ip)
;;   (port-count-lines! ip)
;;   (define my-lexer
;;     (lexer-src-pos
;;      [(repetition 1 +inf.0 numeric) (token 'INTEGER (string->number lexeme))]
;;      [upper-case                    (token 'STRING lexeme)]
;;      ["b"                           (token 'STRING " ")]
;;      [";"                           (token ";" lexeme)]
;;      [whitespace                    (token 'WHITESPACE lexeme #:skip? #t)]
;;      [(eof)                         (void)]))
;;   (thunk (my-lexer ip)))

;; #lang racket/base
;;
;; (require parser-tools/lex
;;          parser-tools/cfg-parser
;;          syntax/reader
;;          (for-syntax racket/base))
;;
;; (provide lex
;;          parse
;;          token->string)
;;
;; (define (parse src-name in)
;;   (parameterize ([current-source src-name])
;;     (parse-from-lex (lambda () (lex in)))))
;;
;; ;; ----------------------------------------
;; ;; Lexer
;;
;; (define-syntax (at-src stx)
;;   (syntax-case stx ()
;;     [(_ e)
;;      (with-syntax ([start (datum->syntax stx '$1-start-pos)]
;;                    [end   (datum->syntax stx '$n-end-pos)])
;;        #'(datum->syntax #f e (to-srcloc start end) orig-prop))]))
;;
;; (define orig-prop (read-syntax 'src (open-input-bytes #"x")))
;;
;; ;; ----------------------------------------
;; ;; Source locations and error reporting:
;;
;; (define current-source (make-parameter #f))
;;
;; (define (to-srcloc start end)
;;   (list
;;    (current-source)
;;    (position-line start)
;;    (position-col start)
;;    (position-offset start)
;;    (and (position-offset end)
;;         (position-offset start)
;;         (- (position-offset end)
;;            (position-offset start)))))
;;
;; (define (raise-parse-error t v start end)
;;   (apply (if (eq? t 'EOF) raise-read-eof-error raise-read-error)
;;          (format "bad syntax at ~a" (token->string t v))
;;          (to-srcloc start end)))
;;
;; (define (token->string t v)
;;   (if v
;;       (format "~a" v)
;;       (format "~a" t)))

(module+ test
  (require "parser.rkt")

  (require rackunit)

  (define (p s)
    ;; (require racket/pretty)
    ;; (pretty-print (syntax->datum (parse (tokenize (open-input-string s)))))
    (syntax->datum (parse (tokenize (open-input-string s)))))

  (check-equal? (p "λx.x\n")
                '(prog (expr (func #f (name x) #f (body (expr (name x)))))))

  (check-equal? (p "λs.(s s)\n")
                '(prog
                  (expr
                   (func
                    #f
                    (name s)
                    #f
                    (body (expr (appl #f
                                      (lexp (expr (name s)))
                                      (aexp (expr (name s))) #f)))))))

  (check-equal? (p "λx.x\nλs.(s s)")
                '(prog
                  (expr (func #f (name x) #f (body (expr (name x)))))
                  (expr
                   (func
                    #f
                    (name s)
                    #f
                    (body (expr (appl #f
                                      (lexp (expr (name s)))
                                      (aexp (expr (name s))) #f)))))))

  (check-equal? (p "λx.xλs.(s s)")
                '(prog
                  (expr (func #f (name x) #f (body (expr (name x)))))
                  (expr
                   (func
                    #f
                    (name s)
                    #f
                    (body (expr (appl #f
                                      (lexp (expr (name s)))
                                      (aexp (expr (name s))) #f)))))))

  'done)
