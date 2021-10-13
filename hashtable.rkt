#lang racket

(define (unroll-addhint code liste)
  (match code
    ((list 'add-hint n s) (values
                           `(ignore (add-hint ,n ,s))
                           (append liste (for/list ((i (in-range n))) (string->symbol (~a s i))))))
    ((cons a b) (begin
                  (define-values (aa la) (unroll-addhint a liste))
                  (define-values (bb lb) (unroll-addhint b la))
                  (values (cons aa bb) lb)))
    (ligne (values ligne liste))
  ))

(unroll-addhint '(add-hint 2 "o") '())
(unroll-addhint '(define tmp (cons (add-hint 2 "o") 4)) '())

; déroule les opérations de hash, notamment en ajoutant les hints
(define (unroll-hashtable-operation code nb)
  (match code
    ; expanding hashtable calls
    ;todo faire ça bien. lookup 2 hints value&pos, add/modify/delete 1hint pos
    ; lookup
    ((list fonction m a)
     #:when (equal? (~a fonction) "hashmap-lookup")
     (begin
       (define-values (mm anb) (unroll-hashtable-operation m (+ nb 2))) ; 2 est le nombre d'hint que je rajoute
       (define-values (aa bnb) (unroll-hashtable-operation a anb))
       (define name-hint (format "~a_~a" (string-join (string-split (~a code) " ") "_") nb))
       (define hint0 (string->symbol (~a name-hint 0)))
       (define hint1 (string->symbol (~a name-hint 1)))
       (values
        `(cons (add-hint 2 ,name-hint) (call ,fonction (,mm ,aa ,hint0 ,hint1)))
        bnb)))
    ; add modify
    ((list fonction map_ key_ value_)
     #:when (or (equal? (~a fonction) "hashmap-add") (equal? (~a fonction) "hashmap-modify")) ;todo delete pas besoin de value
     (begin
       (define-values (map res1) (unroll-hashtable-operation map_ (+ nb 1))) ; On rajoute 1 hint
       (define-values (key res2) (unroll-hashtable-operation key_ res1))
       (define-values (value res) (unroll-hashtable-operation value_ res2))
       (define name-hint (format "~a_~a" (string-join (string-split (~a code) " ") "_") nb))
       (define hint (string->symbol (~a name-hint 0)))
       (values
        `(cons (add-hint 1 ,name-hint) (call ,fonction (,map ,key ,value ,hint)))
        res)))
    ; delete
    ((list fonction map_ key_)
     #:when (equal? (~a fonction) "hashmap-delete") ;todo delete pas besoin de value
     (begin
       (define-values (map res1) (unroll-hashtable-operation map_ (+ nb 1))) ; On rajoute 1 hint
       (define-values (key res) (unroll-hashtable-operation key_ res1))
       (define name-hint (format "~a_~a" (string-join (string-split (~a code) " ") "_") nb))
       (define hint (string->symbol (~a name-hint 0)))
       (values
        `(cons (add-hint 1 ,name-hint) (call ,fonction (,map ,key ,hint)))
        res)))
    ; recursion
    ((cons a b) (begin
                  (define-values (aa anb) (unroll-hashtable-operation a nb))
                  (define-values (bb bnb) (unroll-hashtable-operation b anb))
                  (values (cons aa bb) bnb)))
                  
    ; end
    (_ (values code nb))
    ))

(unroll-hashtable-operation '((define a 2) (define b (hashtable-lookup (hashtable-lookup mm 3) 2))) 0)

; branche tout ensemble
(define (unroll-addhint-final fonction (is-public-fonction #t))
  (define liste-args (second fonction))
  (define len (length liste-args))
  (define-values (liste-commande _) (unroll-hashtable-operation (drop fonction 2) 0)) ; on drop 'public et la liste des arguments
  (define-values (new-liste args) (unroll-addhint liste-commande '()))
  (append (list 'public (append liste-args args)) (append new-liste
                                                          ; on sauvegarde notre valeur de retour avant de tout drop
                                                          (if is-public-fonction (list "OP_TOALTSTACK") '())
                                                          (for/list ((elem-a-drop args)) `(drop ,elem-a-drop))
                                                          (if is-public-fonction (list "OP_FROMALTSTACK") '()))))

; on ne drop pas la dernière valeur quand on unroll avec vyper
(define (unroll-addhint-final-vyper fonction) (unroll-addhint-final fonction #f))

; export
(provide (all-defined-out))


(unroll-addhint-final '(public (args) (define a 2) (define b (hashtable-lookup (hashtable-lookup mm 3) 2))))
