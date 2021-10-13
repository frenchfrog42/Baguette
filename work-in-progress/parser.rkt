#lang racket/base

;; An interactive calculator inspired by the calculator example in the bison manual.

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(require "test.rkt")

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP + - * / ^ EOF NEG CREATE PUBLIC REQUIRE VIRGULE POINTVIRGULE OCROCHET CCROCHET FREE +BYTES DESTROY CHECKEQUAL))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))

  (upper-letter (:/ #\A #\Z))

  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))

(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\newline) (calcl input-port)]
   ;; (token-newline) returns 'newline
   ;[#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["create" 'CREATE]
   ["free" 'FREE]
   ["public" 'PUBLIC]
   ["require" 'REQUIRE]
   ["," 'VIRGULE]
   [";" 'POINTVIRGULE]
   ["{" 'OCROCHET]
   ["}" 'CCROCHET]
   ["+." '+BYTES]
   ["destroy" 'DESTROY]
   ["==" 'CHECKEQUAL]
   
   [(:or "getScriptCode" "bin2num" "bytes-get-last" "bytes-delete-last" "num2bin" "buildOutput" "hash256" "hashOutputs") (token-FNCT lexeme)]
   
   [(:+ (:or lower-letter upper-letter "-" "_")) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))



;(define compteur '(public (tx-arg amount-arg)
;                          (define scriptCode (call getScriptCode (tx-arg)))
;                          (define counter (call bin2num (bytes-get-last scriptCode 1)))
;                          (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode) 1) (+ 1 (destroy counter))))
;                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
;                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
;                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
;                    ))



(define parser-errors
  (hash
   '((0 . #f))
   "missing lhs of eq"
   '((18 . #f) (6 . x) (0 . #f))
   "missing rhs of eq"
   '((12 . #f) (3 . 1) (0 . #f))
   "missing rhs of plus"
   '((3 . 1) (0 . #f))
   "missing left parenthesis"
   '((20 . 1) (8 . #f) (0 . #f))
   "missing right parenthesis"))

(define (my-plus a b)
  (begin
    (printf "~a + ~a ~n" a b)
    (+ a b)))

(define calcp
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error
    (lambda (tok-ok? tok-name tok-value #:stack se)
      (define pe
        (hash-ref parser-errors se #f))
      (if pe (error 'calc pe)
          (error
           'calc
           "Unexpected token: ~a~a\nparser-state = ~v"
           tok-name
           (if tok-value
               (format "(~a)" tok-value)
               "")
           se))))

   (precs (right =)
          (left - + +BYTES)
          (left * /)
          (left NEG)
          (right ^))

   (grammar

    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(PUBLIC OP args CP OCROCHET body CCROCHET) (append (list 'public) (list $3) $6)])

    (args [() '()]
          [(VAR) (list $1)]
          [(VAR VIRGULE args) (append (list $1) $3)])
    
    (args-fcnt [() '()]
               [(exp) (list $1)]
               [(exp VIRGULE args-fcnt) (append (list $1) $3)])
    
    (body [(exp POINTVIRGULE body) (append (list $1) $3)]
          [(var-exp POINTVIRGULE body) (append (list $1) $3)]
          [(require-exp) (list $1)])

    (var-exp [(CREATE VAR) `(define ,$2 0)]
             [(CREATE VAR = exp) `(define ,$2 ,$4)]
             [(FREE VAR) `(drop ,$2)]
             [(VAR = exp) `(cons (define tmp ,$3) (cons (drop ,$1) (define ,$1 (destroy tmp))))])

    (require-exp [(REQUIRE OP exp CP POINTVIRGULE) $3])

    (exp [(OP exp CP) $2]
         [(NUM) $1]
         [(VAR) $1]
         [(DESTROY VAR) `(destroy ,$2)]
         [(FNCT OP args-fcnt CP) `(call ,$1 ,$3)]
         [(exp + exp) `(+ ,$1 ,$3)]
         [(exp CHECKEQUAL exp) `(= ,$1 ,$3)]
         [(exp +BYTES exp) `(+bytes ,$1 ,$3)]
         ;[(exp - exp) (- $1 $3)]
         [(exp * exp) `(* ,$1 ,$3)]))))
;[(exp / exp) (/ $1 $3)]
;[(- exp) (prec NEG) (- $2)]))))

;; run the calculator on the given input-port
(define (calc ip)
  (port-count-lines! ip)
  (let one-line ()
    (define result
      (calcp (lambda () (calcl ip))))
    (when result
      (printf "~a\n" result)
      (one-line))))

(require rackunit
         racket/port)
(define (run s)
  (with-output-to-string
    (λ ()
      (calc (open-input-string s)))))


(run
"public (tx-arg, amount-arg) {
  create scriptCode = getScriptCode(tx-arg);
  create counter = bin2num(bytes-get-last(scriptCode, 1));
  create scriptCode_ = bytes-delete-last(destroy scriptCode, 1) +. (destroy counter + 1);
  create newAmount = num2bin(destroy amount-arg, 8);
  create output = buildOutput(destroy scriptCode_, destroy newAmount);
  require (hash256(destroy output) == hashOutputs(destroy tx-args));
}")
;}")


;(define compteur '(public (tx-arg amount-arg)
;                          (define scriptCode (call getScriptCode (tx-arg)))
;                          (define counter (call bin2num (bytes-get-last scriptCode 1)))
;                          (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode) 1) (+ 1 (destroy counter))))
;                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
;                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
;                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
;                    ))
;(define compteur 'public (tx-arg amount-arg)
;                          (define scriptCode (call getScriptCode (tx-arg)))
;                          (define counter (call bin2num ((call bytes-get-last (scriptCode 1)))))
;                          (define scriptCode_ (+bytes (call bytes-delete-last ((destroy scriptCode) 1)) (+ (destroy counter) 1)))
;                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
;                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
;                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-args)))))

 ; (module+ main
 ;(calc (current-input-port)))