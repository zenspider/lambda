#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/cfg-parser
         syntax/readerr
         (for-syntax racket/base))

(provide lex
         parse
         token->string)

(define (parse src-name in)
  (parameterize ([current-source src-name])
    (parse-from-lex (lambda ()
                      ;; Discard whitespace from `lex`:
                      (let loop ()
                        (let ([v (lex in)])
                          (if (eq? 'WHITESPACE (position-token-token v))
                              (loop)
                              v)))))))

;; ----------------------------------------
;; Lexer

(define-tokens content-tokens
  (NAME ERROR))

(define-empty-tokens delim-tokens
  (EOF LAMBDA OPEN CLOSE DOT WHITESPACE))

(define-lex-abbrevs                     ; stolen from ragg
  [letter  (:or (:/ "a" "z") (:/ #\A #\Z))]
  [digit   (:/ #\0 #\9)]
  [id-char (:or letter digit (char-set "-!$%&/<=>?^_~@"))])

(define-lex-abbrev id
  (:& (complement (:+ digit))
      (:+ id-char)))

(define lex
  (lexer-src-pos
   [id              (token-NAME (string->symbol lexeme))]
   ["λ"             'LAMBDA]
   ["("             'OPEN]
   [")"             'CLOSE]
   ["."             'DOT]
   [(:+ whitespace) 'WHITESPACE]
   [(eof)           'EOF]
   ))

(define parse-from-lex
  (cfg-parser
   (start <prog>)
   (end EOF)

   (tokens content-tokens delim-tokens)

   (error (lambda (a t v start end) (raise-parse-error t v start end)))
   (src-pos)

   (grammar
    (<prog> [()                         null]
            [(<expr> <prog>)            (at-src (cons $1 $2))])
    (<expr> [(<name>)                   $1]
            [(<func>)                   $1]
            [(<appl>)                   $1])
    (<name> [(NAME)                     $1])
    (<func> [(LAMBDA <name> DOT <body>) (at-src `(lambda (,$2) ,$4))])
    (<appl> [(OPEN <lexp> <aexp> CLOSE) (at-src `(,$2 ,$3))])
    (<body> [(<expr>)                   $1])
    (<lexp> [(<expr>)                   $1])
    (<aexp> [(<expr>)                   $1])
    )))

(define-syntax (at-src stx)
  (syntax-case stx ()
    [(_ e)
     (with-syntax ([start (datum->syntax stx '$1-start-pos)]
                   [end   (datum->syntax stx '$n-end-pos)])
       #'(datum->syntax #f e (to-srcloc start end) orig-prop))]))

(define orig-prop (read-syntax 'src (open-input-bytes #"x")))

;; ----------------------------------------
;; Source locations and error reporting:

(define current-source (make-parameter #f))

(define (to-srcloc start end)
  (list
   (current-source)
   (position-line start)
   (position-col start)
   (position-offset start)
   (and (position-offset end)
        (position-offset start)
        (- (position-offset end)
           (position-offset start)))))

(define (raise-parse-error t v start end)
  (apply (if (eq? t 'EOF) raise-read-eof-error raise-read-error)
         (format "bad syntax at ~a" (token->string t v))
         (to-srcloc start end)))

(define (token->string t v)
  (if v
      (format "~a" v)
      (format "~a" t)))

(module+ main
  (require racket/generator)
  (require racket/sequence)

  (define (tokenize str)
    (define ip (open-input-string str))
    (port-count-lines! ip)
    (for/list ([x (stop-after (in-producer (lambda () (lex ip)))
                              (lambda (t) (eq? 'EOF (position-token-token t))))]
               #:unless (eq? 'WHITESPACE (position-token-token x)))
      x))

  (syntax->datum (parse-from-lex (sequence->generator (tokenize "λx.x\n"))))
  (syntax->datum (parse-from-lex (sequence->generator (tokenize "λs.(s s)\n"))))
  (syntax->datum (parse-from-lex (sequence->generator (tokenize "λx.x\nλs.(s s)"))))
  (syntax->datum (parse-from-lex (sequence->generator (tokenize "λx.xλs.(s s)"))))

  (syntax->datum (parse 'test (open-input-string "λx.x")))

  )
