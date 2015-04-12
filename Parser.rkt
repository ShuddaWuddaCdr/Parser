#lang racket

; Code taken/altered from http://matt.might.net/articles/lexers-in-racket/ to get us started/get ideas

; Get parser tools from Racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

; Provide all definitions in this file
(provide (all-defined-out))


(define (string->char s)
  (car (string->list s)))


(define test-exp1 (open-input-string "simplify x+2x+3"))
(define test-exp2 (open-input-string "x + 2 * x + 3"))

; Basic Parser
(define lex-char
  (lexer
    ; skip spaces:
    (#\space     (lex-char input-port))
    
    ; skip newline:
    (#\newline   (lex-char input-port))
   
    ; an actual character:
    (any-char    (list 'CHAR (string->char lexeme)))))


; Calc lex example

; identifiers:  [a-zA-Z]+
; delimiters:   "("|")"
; operators:    "+"|"*"
; integers:     -?[0-9]+
; whitespace:   [ \n]+

(define calc-lexer
  (lexer
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    ; =>
    (cons `(ID ,(string->symbol lexeme))
          (calc-lexer input-port))]
   
   [#\( 
    ; =>
    (cons '(LPAR)
          (calc-lexer input-port))]
   
   [#\)
    ; =>
    (cons '(RPAR) 
          (calc-lexer input-port))]
   
   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    ; =>
    (cons `(INT ,(string->number lexeme))
          (calc-lexer input-port))]
   
   [(:or #\+ #\*)
    ; =>
    (cons `(OP ,(string->symbol lexeme))
          (calc-lexer input-port))]
   
   [whitespace 
    ; =>
    (calc-lexer input-port)]
   
   [(eof)
    '()]))

; Simple lexer loop
(define (run-lexer port)
  (when (not (eq? 'eof (calc-lexer port)))
      (calc-lexer port)))