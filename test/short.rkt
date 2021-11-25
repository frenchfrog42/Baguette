#lang racket

(require rackunit)
(require "../compilation.rkt")
(require "../util.rkt")

; takes a stack, code, and check it's the result
(define (test stack code result)
  (displayln (append '(public) (list stack) code))
  (define res (contract->opcodes (append '(public) (list stack) code)))
  (displayln res)
  (if (string=? res result) '() (exit 1)))

; simple test
;(test '() '(1) "OP_1") ;todo sans arg ça fail
(test '(a) '(1) "OP_1")

; simple expression to test optimisation
(test '(a) '((+ 1 (destroy a))) "OP_1ADD")
(test '(a) '((+ (destroy a) 1)) "OP_1ADD")
(test '(a) '((- (destroy a) 1)) "OP_1SUB")

(test '(a b) '((+ (destroy a) (destroy b))) "OP_ADD")
(test '(a b) '((+ (destroy b) (destroy a))) "OP_ADD")

; (test '(a) '((modify a a)) "") todo
; (test '(a) '((modify a 0)) "OP_DROP OP_0") ;todo 0 swap drop

; todo check order of args
;(test '(a b) '((+ 1 (destroy a))) "OP_1ADD")
(test '(a b c d) (list (apply-all '((destroy a) (destroy b) (destroy c) (destroy d)) '+)) "OP_ADD OP_ADD OP_ADD")
;(test '(c a d b) (list (apply-all '((destroy a) (destroy b) (destroy c) (destroy d)) '+)) "OP_ADD OP_ADD OP_ADD OP_ADD") ;todo one day
; todo make + a n-ary operator and test all permutations

; 0 use of a in modify
(test '(a) '((modify a 2)) "OP_DROP OP_2")
; 1 use of a in modify
(test '(a) '((modify a (+ 1 a))) "OP_1ADD")
(test '(a) '((modify a (+ a 1))) "OP_1ADD")
; 2 use of a in modify
; correct for now, but not really optimal
;(test '(a) '((modify a (+ a a))) "OP_DUP OP_OVER OP_ADD OP_NIP")

(compile-expr-all '(call hash256 (2))) ;l'api a call est mal faite
(compile-expr-all '(+ 1 2))


