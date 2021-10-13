#lang racket

(define (apply-all l symbol)
  (match l
    ((cons a '()) a)
    ((cons a b) (list symbol a (apply-all b symbol)))))

(define cons* (lambda (l) (apply-all l 'cons)))
(define +bytes* (lambda (l) (apply-all l '+bytes)))

(define (hex-int n)
  (if (< n 11) (format "~a" n)
      (match n
        (11 "a")
        (12 "b")
        (13 "c")
        (14 "d")
        (15 "e")
        (16 "f"))))

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

; export
(provide (all-defined-out))
