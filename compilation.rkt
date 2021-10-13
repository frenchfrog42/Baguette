#lang racket

(require "util.rkt")

; stack used for the compilation
(define stack '())
; if the compilation is approx (if we create stack var out of thin air)
(define approx #f)

(define (set-approx val)
  (set! approx val))

(define (compile-int n)
  (if (< n 17)
      (list (~a "OP_" n))
      (list (format "~a~a" (hex-int (quotient n 16)) (hex-int (remainder n 16))))))

(define (to-opcode s)
  (match s
    ;pick
    ("pick0" '("OP_DUP"))
    ("pick1" '("OP_OVER"))
    ((regexp #rx"pick*") (list (~a "OP_" (substring s 4)) "OP_PICK"))
    ;roll
    ("roll0" '("//roll0"))
    ("roll1" '("OP_SWAP"))
    ("roll1" '("OP_ROT"))
    ((regexp #rx"roll*") (list (~a "OP_" (substring s 4)) "OP_ROLL"))
    ;drop
    ("drop0" '("OP_DROP"))
    ("drop1" '("OP_NIP"))
    ((regexp #rx"drop*") (list (~a "OP_" (substring s 4)) "OP_ROLL" "OP_DROP"))
    (s s)))

;todo
(define-syntax rotate
  (syntax-rules ()
    [(rotate a) (cons (debug-table a) '())]
    [(rotate a b c ...) (cons (debug-table a)
                          (rotate b c ...))]))

(define-syntax-rule (debug-table l)
  `(,(first 'l) ,(format "~a (~a)" (first 'l) (second 'l))))
  
(define table-binary-op
  (append '(
     (+ "OP_ADD")
     (- "OP_SUB")
     (* "OP_MUL")
     (/ "OP_DIV")
     (= "OP_EQUAL")
     (< "OP_LESSTHAN")
     (& "OP_AND")
     (&& "OP_AND")
  ; )(rotate
     (bytes-get-first "OP_SPLIT drop0")
     (bytes-get-last "roll1 OP_SIZE roll2 OP_SUB OP_SPLIT drop1") ; todo
     (bytes-delete-first "OP_SPLIT drop1")
     (bytes-delete-last "roll1 OP_SIZE roll2 OP_SUB OP_SPLIT drop0") ; todo
     (+bytes "OP_CAT")
)))

(define (compile-op op)
  (cadr (binary-op? op)))

(define (binary-op? op)
  (findf (lambda (arg) (eq? op (car arg))) table-binary-op))

(define (compile-var var)
  (list (~a "pick" (index-of stack var))))

(define (compile-destroy-var var)
  (let ((res (list (~a "roll" (index-of stack var))))) (delete-var var) res))

(define (new-var x)
  (set! stack (cons x stack)))

(define (delete-var x)
  (set! stack (remove x stack)))

(define (push-stack)
  (set! stack (cons (gensym) stack)))

(define (drop-stack)
  (set! stack (cdr stack)))

(define (set-stack s)
  (set! stack s))

(define-syntax-rule (save-stack e)
  (let ((stackbis (gensym)))
   (set! stackbis stack)
   (let ((res e))
     (set! stack stackbis)
     res)))

(define-syntax-rule (freeze-stack e)
  (let ((stackbis (gensym)))
   (set! stackbis stack)
   (let ((res e))
     (if (eq? stack stackbis) res
         (begin
           (println stack)
           (println stackbis)
           (/ 1 0))))))

(define-syntax-rule (incr-stack e)
  (let ((stackbis (gensym)))
   (set! stackbis stack)
   (let ((res e))
     (set! stackbis (cdr stackbis))
     (if (eq? stack stackbis) res
         (begin
           (println stack)
           (println stackbis)
           (/ 1 0))))))

(define (naive-opt expr)
  (match expr
    ((list* "OP_SWAP" "OP_ADD" reste) (naive-opt (list* "OP_ADD" reste)))
    ((list* "OP_SWAP" "OP_MUL" reste) (naive-opt (list* "OP_MUL" reste)))
    ((cons a b) (cons a (naive-opt b)))
    ('() '())))

(define (compile-binary-op-all-no-commute op expr)
  (append
   ; a b +
   (let ((un (compile-expr-all (first expr))))
     (push-stack)
     (let ((deux (compile-expr-all (second expr))))
       (drop-stack)
       (define all-comb (cartesian-product un deux))
       (let ((res (for/list ((e all-comb)) (append (first e) (second e) (list op)))))
         ;(displayln res)
         res)))))

(define (compile-binary-op-all op expr)
  ; We want to compile (+ a b)
  ; Here is all the way to compile it
  (append
   ; a b +
   (save-stack (let ((un (compile-expr-all (first expr))))
                 (push-stack)
                 (let ((deux (compile-expr-all (second expr))))
                   (drop-stack)
                   (define all-comb (cartesian-product un deux))
                   (let ((res (for/list ((e all-comb)) (append (first e) (second e) (list op))))) res))))
   ; b a +
   (save-stack (let ((deux (compile-expr-all (second expr))))
                 (push-stack)
                 (let ((un (compile-expr-all (first expr))))
                   (define all-comb (cartesian-product deux un))
                   (let ((res (for/list ((e all-comb)) (append (first e) (second e) (list op))))) res))))
   ; a TOALTSTACK b FROMALTSTACK +
   ; Same result as
   ; b TOALTSTACK a +
   (let ((un (compile-expr-all (first expr)))
         (deux (compile-expr-all (second expr))))
     (define all-comb (cartesian-product deux un))
     (let ((res (for/list ((e all-comb)) (append (first e) (list "OP_TOALTSTACK") (second e) (list "OP_FROMALTSTACK") (list op))))) res))))

; Prends une liste de liste d'option et forme le produit
(define (recursive-product l)
  (match l
    ((cons a reste)
     (define recursion (recursive-product reste))
     ;(printf "~a ~a~n" reste recursion)
     (for*/list ((option a) (comb recursion)) (cons option comb)))
    (a (list a))))

; (recursive-product '((0)))
; (recursive-product '((0) (1)))
; (recursive-product '((0 1) (2)))
; (recursive-product '((0 1) (2 3)))
; (recursive-product '((0 1) (2 3) (4 5)))

(define (all-args-permutations e)
  (begin ;(displayln e)
  (match e
    ((list 'public args expr ...)
     (foldl append '()
      (for/list ((perm-arg (permutations args)))
        (save-stack (let ()
         (for-each new-var perm-arg)
         (define all-expr-perm (for/list ((e expr)) (compile-expr-all e)))
         (define iter-all-expr (recursive-product all-expr-perm))
         ;(printf "All combos ~a~n" iter-all-expr)
         (define all-contract-perms (for/list ((version iter-all-expr))
                                              (let ((res (list* (format "~a" (stack->string perm-arg)) version))) ;(compile-list-public-function version))))
                                                ;(printf "Res: ~a~n" res)
                                                res)))
         all-contract-perms
     ))))))))


(define (compile-contract-all l)
  ; l is a list of public function
  ; We want to select on index
  ; The code produced is
  ; pick0, equal0, drop, drop, repeat
  (list (flatten (append
   (for/list ((expr l) (i (in-range (length l))))
     (begin ;(displayln expr)
             (append
              (car (compile-expr-all (list '= "pick0" i)))
              (list "OP_IF" "drop0")
              (begin (printf "~n~nlisteargs? ~a~n~n" (car (last (all-args-permutations expr)))) '())
              (flatten (cdr (last (all-args-permutations expr))))
              (list "OP_ELSE"))))
   (list "OP_0")
   (for/list ((i (in-range (length l)))) "OP_ENDIF")))))

(define (compile-expr-all e)
  (begin
   ;(printf "Expr: ~a Stack: ~a~n" e stack)
   (let ((res 
   (match e
     ; int
     (expr #:when (number? expr) (list (compile-int expr)))
     ; call. Idem rajouter des elements sur la stack
     ((list 'call nom args) (list (append
                                   ;(for/list ((expr args)) (car (compile-expr-all expr)))
                                   (call-call nom args)))) ; todo remplacer et mettre juste "(call-call nom args)" et voir si ça casse rien
     ; define
     ((list 'define var expr) (let ((res (compile-expr-all expr))) (new-var var) res)) ; todo
     ; binary expression
     ((cons op reste) #:when (eq? op '+) (compile-binary-op-all (compile-op op) reste))
     ((cons op reste) #:when (binary-op? op) (compile-binary-op-all-no-commute (compile-op op) reste))
     ; when in approximation mode
     (var #:when approx '(("pick10")))
     ; var
     (var #:when (and (symbol? var) (member var stack))
          (list (compile-var var)))
     ; var destroy
     ((list 'destroy var) #:when (and (symbol? var) (member var stack))
                          (list (compile-destroy-var var)))
     ((list 'drop var) #:when (and (symbol? var) (member var stack))
                       (list (append (compile-destroy-var var) '("OP_DROP"))))
     ((list 'destroy var) (printf "Use of destroyed variable ~a with stack ~a. This is invalid\n" var stack))
     ; modify
     ((list 'modify var expr) (compile-expr-all `(cons (define tmp-var-modify ,expr) (cons (drop ,var) (define ,var (destroy tmp-var-modify))))))
     ; copy
     ((list 'copy var) #:when (and (symbol? var) (member var stack))
                       (list (compile-var var)))
     ; contract
     ((list 'contract liste ...) (begin (printf "'contract avec ~a~n" liste) (compile-contract-all liste)))
     ((list 'public args expr ...) (all-args-permutations e))
     ; verify expression
     ((list 'verify l)
      (define all-expr (compile-expr-all l))
      (for/list ((e all-expr)) (append e (list "OP_VERIFY"))))
     ; if ; todo
     ((list 'if cond true false)
      (for/list ((mycond (compile-expr-all cond))
            (mytrue (save-stack (compile-expr-all true)))
            (myfalse (compile-expr-all false)))
        (append
         mycond
         (list "OP_IF")
         mytrue
         (list "OP_ELSE")
         myfalse
         (list "OP_ENDIF"))))
     ; cons
     ((list 'cons a b) (for/list ((aa (compile-expr-all a))
                                  (bb (compile-expr-all b)))
                         (append aa bb)))
     ; debug
     ('debug (begin (println (format "STAAAAAAAAACK: ~a" stack)) '(())))
     ; ignore
     ((list 'ignore a) '(()))
     ; reste
     (e (list (list e))))))
   ;(printf "FINN. Expr: ~a Stack: ~a~n" e stack)
  res)))

(define (toLEUnsigned n l)
  (car (compile-expr-all (cons* `((define tmp-m (call num2bin (,n (+ ,l 1))))
                                  (define taille-tmp-m (call getLen (tmp-m)))
                                  (bytes-get-first (destroy tmp-m) (- (destroy taille-tmp-m) 1)))))))
(define (fromLEUnsigned bytes)
  (append (car (compile-expr-all `(+bytes ,bytes "00"))) (list "OP_BIN2NUM")))

(define (readVarint tx)
  ; il y avait un freeze-stack
  (car (compile-expr-all
                    `(cons (define tx ,tx)
                           (cons (define header (bytes-get-first tx 1))
                                 (if (= header "fd")
                                     (cons (define l (call fromLEUnsigned (bytes-delete-first (bytes-get-first tx 3) 1)))
                                           (cons (drop header)
                                                 (bytes-get-first (bytes-delete-first (destroy tx) 3) (destroy l))))
                                     (if (= header "fe")
                                         (cons (define l (call fromLEUnsigned (bytes-delete-first (bytes-get-first tx 5) 1)))
                                               (cons (bytes-get-first (bytes-delete-first (destroy tx) 5) (destroy l))
                                                     (drop header)))
                                         (if (= header "ff")
                                             (cons (define l (call fromLEUnsigned (bytes-delete-first (bytes-get-first tx 9) 1)))
                                                   (cons (bytes-get-first (bytes-delete-first (destroy tx) 9) (destroy l))
                                                         (drop header)))
                                             (cons (define l (call fromLEUnsigned (destroy header)))
                                                   (bytes-get-first (bytes-delete-first (destroy tx) 1) (destroy l)))))))))))

(define (readVarint-compile-small tx)
  (begin
    ;(set! a-verifier (lambda (size) (< size 200)))
    (compile-expr-all
     `(cons (define tx ,tx)
            (cons (define header (bytes-get-first tx 1))
                  (cons (define l (call fromLEUnsigned (destroy header)))
                        (bytes-get-first (bytes-delete-first (destroy tx) 1) (destroy l))))))))

(define (writeVarint tx)
  (car (compile-expr-all
        `(cons (define txici ,tx)
               (cons (define n (call getLen (txici)))
                     (if (< n "fd00")
                         (+bytes (call toLEUnsigned ((destroy n) 1)) (destroy txici)) ;todo
                         (+bytes (+bytes "fd" (call toLEUnsigned ((destroy n) 2))) (destroy txici))))))))

(define (writeVarint-compile-small tx)
  (compile-expr-all
   `(cons (define txici ,tx)
          (cons (define header (call getLen (txici))) ;size
                (+bytes (bytes-get-first (destroy header) 1) (destroy txici))))))

(define (getScriptCode tx)
  ;(car (compile-expr-all `(call readVarint-compile-small (bytes-delete-first ,(car tx) 104)))))
  (car (compile-expr-all `(call readVarint (bytes-delete-first ,(car tx) 104)))))

(define (getScriptCode-with-header tx)
  (car (compile-expr-all `(bytes-get-first (bytes-delete-first ,(car tx) 104) "s1"))))

(define (buildOutput args)
  ;(car (compile-expr-all `(+bytes ,(second args) (call writeVarint-compile-small (,(first args)))))))
  (car (compile-expr-all `(+bytes ,(second args) (call writeVarint (,(first args)))))))

(define (buildOutput-with-header args)
  (car (compile-expr-all `(+bytes ,(second args) ,(first args)))))

(define (hashOutputs args)
  (car (compile-expr-all `(cons (define abc ,(car args))
                                (bytes-get-last (bytes-delete-last (destroy abc) 8) 32)))))

(define (call-call function args)
  (match function
    ('getLen (map (lambda (a) (append a (list '("OP_SIZE OP_NIP")))) (compile-expr-all (car args))))
    ('not (map (lambda (a) (append a (list "OP_NOT"))) (compile-expr-all args)))
    ('getScriptCode (getScriptCode args))
    ('getScriptCode-with-header (getScriptCode-with-header args))
    ('buildOutput (buildOutput args))
    ('buildOutputP2PKH (buildOutput (list (list "OP_DUP OP_HASH160" "14" (first args) "OP_EQUALVERIFY" "OP_CHECKSIG") (second args)))) ; todo à tester
    ('buildOutput-with-header (buildOutput-with-header args))
    ;('buildOutput (list '("arg + call buildOutput")))
    ('hashOutputs (hashOutputs args))
    ;('hashOutputs (list '("arg + call hashOutputs")))
    ('readVarint (readVarint args))
    ('readVarint-compile-small (readVarint-compile-small args))
    ('writeVarint (writeVarint (car args)))
    ('writeVarint-compile-small (writeVarint-compile-small (car args)))
    ('fromLEUnsigned (fromLEUnsigned args))
    ('toLEUnsigned (toLEUnsigned (first args) (second args)))
    ('bin2num (map (lambda (a) (append a (list '("OP_BIN2NUM")))) (compile-expr-all args)))
    ('num2bin (append
               (car (compile-expr-all (first args)))
               (car (compile-expr-all (second args)))
               (list '("OP_NUM2BIN"))))
    ('checksigverify (append
                      (car (compile-expr-all (first args)))
                      (car (compile-expr-all (second args)))
                      (list '("OP_CHECKSIGVERIFY"))))
    ('hash256 (map (lambda (a) (append a (list '("OP_HASH256")))) (compile-expr-all (car args))))
    ('sha256 (map (lambda (a) (append a (list '("OP_SHA256")))) (compile-expr-all (car args))))
    ; hashmap
    ; value et hint sont deux hints, donc pas besoin de faire tout ce bordel
    ; en fait pas besoin parcequ'on peut pas appeler avec value et hint qui valent des destroy ; todo etre sur qu'on peut pas
    ('hashmap-lookup (match-let (((list m_ key_ value hint) args) ((list m key) (list (gensym) (gensym))))
                       (compile-expr-all (cons*
                                          `((define ,m ,m_)
                                            (define ,key ,key_)
                                            (define pos (* 64 ,hint))
                                            (define a_i (bytes-get-first (bytes-delete-first ,m pos) 32))
                                            (verify (= (call sha256 ((destroy ,key))) (destroy a_i)))
                                            (define b_i (bytes-get-first (bytes-delete-first (destroy ,m) (+ 32 (destroy pos))) 32))
                                            (verify (= (call sha256 (,value)) (destroy b_i)))
                                            ,value
                                            )))))
    ('hashmap-add (match-let (((list m_ key_ value hint) args) ((list m key) (list (gensym) (gensym))))
                    (compile-expr-all (cons*
                                       `((define ,m ,m_)
                                         (define ,key ,key_)
                                         (define pos (* 64 ,hint))
                                         (define a_i_moins1 (bytes-get-first (bytes-delete-first ,m pos) 32))
                                         (define a_i (call sha256 ((destroy ,key))))
                                         (define a_i_plus1 (bytes-get-first (bytes-delete-first ,m (+ 64 pos)) 32))
                                         (verify (&& (< (destroy a_i_moins1) a_i) (< a_i (destroy a_i_plus1))))
                                         ,(+bytes* `(
                                                     (bytes-get-first ,m (+ 64 pos))
                                                     (destroy a_i)
                                                     (call sha256 (,value))
                                                     (bytes-delete-first (destroy ,m) (+ 64 (destroy pos)))))
                                         )))))
    ('hashmap-modify (match-let (((list m_ key_ value hint) args) ((list m key) (list (gensym) (gensym))))
                    (compile-expr-all (cons*
                                       `((define ,m ,m_)
                                         (define ,key ,key_)
                                         (define pos (* 64 ,hint))
                                         (define a (bytes-get-first (bytes-delete-first ,m pos) 32))
                                         (verify (= (call sha256 ((destroy ,key))) (destroy a)))
                                         ,(+bytes* `(
                                                     (bytes-get-first ,m (+ 32 pos))
                                                     (call sha256 (,value))
                                                     (bytes-delete-first (destroy ,m) (+ 64 (destroy pos)))))
                                         )))))
    ('hashmap-delete (match-let (((list m_ key_ hint) args) ((list m key) (list (gensym) (gensym))))
                    (compile-expr-all (cons*
                                       `((define ,m ,m_)
                                         (define ,key ,key_)
                                         (define pos (* 64 ,hint))
                                         (define a (bytes-get-first (bytes-delete-first ,m pos) 32))
                                         (verify (= (call sha256 ((destroy ,key))) (destroy a)))
                                         ,(+bytes* `(
                                                     (bytes-get-first ,m pos)
                                                     (bytes-delete-first (destroy ,m) (+ 64 (destroy pos)))))
                                         )))))
    ))

;(define (comp-save-all e) (save-stack (compile-expr-all e)))
; contract->ir = compile-expr-all
(define (ir->opcodes c)
  (string-join (filter (lambda (s) (not (equal? "//" (substring s 0 2))))
                                   (flatten (map to-opcode
                                                 (flatten (map (lambda (a) (string-split a " "))
                                                               (flatten c))))))))

(define (opcodes->size c) (length (string-split c " "))) ; todo faire le vrai calcul (et voir si ya un +1 ou pas

(define (contract->opcodes c (keep-args-order #t))
  (set-approx #f) (set-stack '()) ;make sure approx is off and stack is empty
  (define allcomp (map ir->opcodes (compile-expr-all c)))
  (define res (if keep-args-order (last allcomp) (argmin opcodes->size allcomp))) ; todo prendre l'argmin dans le 1er cas aussi         
  (printf "Size of contract: approx ~a bytes~n" (opcodes->size res))
  (println res))

(define (subcontract->size c)
  (set-approx #t)
  (define allcomp (map ir->opcodes (compile-expr-all c)))
  (set-approx #f)
  (opcodes->size (argmin opcodes->size allcomp)))

; automatically drop variable
(define (remove-destroy-variable code)
  (match code
    ((list 'destroy var) var)
    ((cons a b) (cons (remove-destroy-variable a) (remove-destroy-variable b)))
    (_ code)))

(define (collect-list-define code)
  (match code
    ((list 'define var reste) (begin (collect-list-define reste) `(,var))) ;define cannot be included in each others, but destroy can
    ((list 'destroy var) (error "Use of destroyed variable in garbage collected function"))
    ((cons a b) (append (collect-list-define a) (collect-list-define b)))
    (_ '())))

; (collect-list-define '((define a 2) (define b 3)))

(define (drop-all code liste-var (is-public-function #t))
  (append code
          ; save value before dropping everything (todo modify the stack directly instead of dropping
          (if is-public-function '() '("OP_TOALTSTACK"))
          (list (cons* (map (lambda (var) `(drop ,var)) liste-var)))
          (if is-public-function '() '("OP_FROMALTSTACK"))))

(define (garbage-collector code (is-public-function #t))
  (drop-all code
            ;A priori les define les plus récents seront au top, et il faut les supprimer en 1er. Todo faire un truc plus opti ?
            ;(second code) is the list of arguments of the public function
            (append (reverse (collect-list-define code)) (second code))
            is-public-function))

    

; export
(provide (all-defined-out))
