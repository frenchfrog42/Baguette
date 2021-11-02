#lang racket

(define (apply-all l symbol)
  (match l
    ((cons a '()) a)
    ((cons a b) (list symbol a (apply-all b symbol)))))

(define cons* (lambda (l) (apply-all l 'cons)))
(define +bytes* (lambda (l) (apply-all l '+bytes)))

(define (hex-int n)
  (if (< n 10) (format "~a" n)
      (match n
        (10 "a")
        (11 "b")
        (12 "c")
        (13 "d")
        (14 "e")
        (15 "f"))))

(define-syntax-rule (ignore e) '())

(define-syntax-rule (execute e)
  (let ()
  (display "debut") (display "\n")
  (let ((res e))
    (print "fin") (display "\n")
    res)))

(define (stack->string s)
  (define list-of-char (map symbol->string s))
  (format "~a ||"(string-join list-of-char)))

(define (string->stack s)
  (define list-of-char (string-split s))
  (map string->symbol list-of-char))

; Conversion of opcodes to hex. Useful for metaprogramming
(define (opcode->hex a)
  (match a
    ("OP_1" "51")
    ("OP_2" "52")
    ("OP_3" "53")
    ("OP_4" "54")
    ("20" "0120")
    ("40" "0140")
    ("OP_SPLIT" "7f")
    ("OP_SWAP" "7c")
    ("OP_BIN2NUM" "81")
    ("OP_PICK" "79")
    ("OP_EQUAL" "87")
    ("OP_VERIFY" "69")
    ("OP_1ADD" "8b")
    ("OP_ROLL" "7a")
    ("OP_DROP" "75")
    ("OP_ROT" "7b")
    (_ (error (~a "opcode->hex, your opcode " a " is not in the list")))
    ))

; export
(provide (all-defined-out))
