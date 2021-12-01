#lang racket

(require "util.rkt")

; stack used for the compilation
(define stack '())
; if the compilation is approx (if we create stack var out of thin air)
(define approx #f)

(define (set-approx val)
  (set! approx val))

(define (compile-int n)
  (if (integer? n)
      (if (< n 17)
          (list (~a "OP_" n))
          (list (format "~a~a" (hex-int (quotient n 16)) (hex-int (remainder n 16)))))
  "wtf")) ;wtf

(define (to-opcode s)
  (match s
    ;pick
    ("pick0" '("OP_DUP"))
    ("pick1" '("OP_OVER"))
    ((regexp #rx"pick*") (list (compile-int (string->number (substring s 4))) "OP_PICK"))
    ;roll
    ("roll0" '("//roll0"))
    ("roll1" '("OP_SWAP"))
    ("roll2" '("OP_ROT"))
    ((regexp #rx"roll*") (list (compile-int (string->number (substring s 4))) "OP_ROLL"))
    ;drop
    ("drop0" '("OP_DROP"))
    ("drop1" '("OP_NIP"))
    ((regexp #rx"drop*") (list (compile-int (string->number (substring s 4))) "OP_ROLL" "OP_DROP"))
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
     (<= "OP_LESSTHANOREQUAL")
     (& "OP_AND")
     (&& "OP_AND") ;todo
     (<< "OP_LSHIFT")
     (>> "OP_RSHIFT")
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

; Obvious pattern to remove
(define liste-naive-opt '(
                          ; todo extract useful optims from https://github.com/Bitcoin-com/cashscript/blob/master/packages/utils/src/cashproof-optimisations.ts
                          ; stack transformation
                          (#rx"OP_SWAP OP_SWAP " "")
                          (#rx"OP_SWAP OP_DROP" "OP_NIP")
                          ; opcodes that are a shortcort for another
                          (#rx"OP_1 OP_ADD" "OP_1ADD")
                          (#rx"OP_1 OP_SUB" "OP_1SUB")
                          (#rx"OP_DROP OP_DROP" "OP_2DROP")
                          ; commutative op
                          (#rx"OP_SWAP OP_ADD" "OP_ADD")
                          (#rx"OP_SWAP OP_MUL" "OP_MUL")
                          (#rx"OP_SWAP OP_AND" "OP_AND")
                          (#rx"OP_SWAP OP_OR" "OP_OR")
                          (#rx"OP_SWAP OP_XOR" "OP_XOF")
                          ))
(define (naive-opt-aux expr)
  (regexp-replaces expr liste-naive-opt))

(define (naive-opt expr)
  (define new-expr (naive-opt-aux expr))
  (if (string=? new-expr expr) expr (naive-opt new-expr)))

; Compilation of non commutative binary operation
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

; Compilation of commutative binary operation
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
     (let ((res (for/list ((e all-comb)) (append (first e) (list "OP_TOALTSTACK") (second e) (list "OP_FROMALTSTACK") (list op))))) res)))
  )

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

; compile without testing all permutation
(define (compile-function-simple e)
  (begin
  (match e
    ((list 'public args expr ...)
     (foldl append '()
      (for/list ((perm-arg (list args))) ;todo checker si c'est un reverse ou pas ; todo voir vraiment ça
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

; same, but with all permutation
(define (compile-function-all e)
  (begin
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

; by default, use the all permutation
(define compile-function compile-function-all)


(define (compile-contract-all l (keep-args-order #t))
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
              (let ()
                (define allcomp (map naive-opt (map ir->opcodes (compile-expr-all expr)))) ; ir->opcodes and naive opt
                (define opt-res (argmin opcodes->size allcomp))
                (define res (if keep-args-order (argmin opcodes->size (filter (lambda (c) (equal? (first (string-split c " || ")) (first (string-split (last allcomp) " || ")))) allcomp)) opt-res))
                (define code-function (second (string-split res " || ")))
                (printf "Order of arguments for function n°~a: ~a~n" i (first (string-split res " || ")))
                (string-split code-function))
              ;(flatten (cdr (last (compile-function-all expr))))
              (list "OP_ELSE"))))
   (list "OP_0")
   (for/list ((i (in-range (length l)))) "OP_ENDIF")))))

; for modify
(define (compte-var code var)
  (define this (lambda (c) (compte-var c var)))
  (match code
    (v #:when (eq? v var) 1)
    ((cons a b) (+ (this a) (this b)))
    (_ 0)))
(define (replace-var-by-destroy code var)
  (define this (lambda (c) (replace-var-by-destroy c var)))
  (match code
    (v #:when (eq? v var) (list 'destroy var))
    ((cons a b) (cons (this a) (this b)))
    (_ code)))
(define (replace-var-by-destroy-once code var)
  (define this (lambda (c) (replace-var-by-destroy-once c var)))
  (match code
    (v #:when (eq? v var) (list 'destroy var))
    ((cons a b) (let ((res-a (this a)))
                  (if (eq? a res-a)
                      (cons a (this b))
                      (cons res-a b))))
    (_ code)))


(define/contract (compile-expr-all e)
  (unconstrained-domain-> (lambda (elem) (and (list? elem) (or (empty? elem) (list? (first elem))))))
  (begin
   ;(printf "Expr: ~a Stack: ~a~n" e stack)
   (let ((res 
   (match e
     ; int
     (expr #:when (number? expr) (list (compile-int expr)))
     ; call. Idem rajouter des elements sur la stack
     ((list 'call nom args) (call-call nom args)) ; todo remplacer et mettre juste "(call-call nom args)" et voir si ça casse rien
     ; define
     ((list 'define var expr) (let ((res (compile-expr-all expr))) (new-var var) res)) ; todo
     ;((list 'push-var var) (new-var var) res)) ; todo make a expression to push a var to the stack when we write assembly
     ; binary expression
     ((cons op reste) #:when (eq? op '+) (compile-binary-op-all (compile-op op) reste))
     ((cons op reste) #:when (binary-op? op) (compile-binary-op-all-no-commute (compile-op op) reste))
     ; var
     (var #:when (and (symbol? var) (member var stack))
          (list (compile-var var)))
     ; var destroy
     ((list 'destroy var) #:when (and (symbol? var) (member var stack))
                          (list (compile-destroy-var var)))
     ((list 'drop var) #:when (and (symbol? var) (member var stack))
                       (list (append (compile-destroy-var var) '("OP_DROP"))))
     ((list 'roll var) #:when (and (symbol? var) (member var stack))
                          (let ((res (list (compile-destroy-var var)))) (new-var var) res))
     ((list 'destroy var) (begin (printf "Use of destroyed variable ~a with stack ~a. This is invalid\n" var stack) '()))
     ; modify
     ((list 'modify-simple var expr) (compile-expr-all `(cons (define tmp-var-modify ,expr) (cons (drop ,var) (define ,var (destroy tmp-var-modify))))))
     ((list 'modify var expr) ;(compile-expr-all `(cons (define tmp-var-modify ,expr) (cons (drop ,var) (define ,var (destroy tmp-var-modify))))))
      (match (compte-var expr var)
        ;if var is in expr only once, change the first expr to a destroy, and execute the expr
        (1 (let ((res (compile-expr-all (replace-var-by-destroy expr var))))
             (new-var var)
             res))
        ; If the var isn't here, we drop the var and execute the expr
        (0 (let ((res (compile-expr-all `(cons (drop ,var) ,expr)))) (new-var var) res))
        ; then, the var is here multiple time
        (_ (append
            ; the first use of this variable is destroyed
            (compile-expr-all `(cons
                               (define tmp-var-modify ,(replace-var-by-destroy-once expr var))
                               (define ,var (destroy tmp-var-modify))))
            ; we use the variable a lot, and destroy it after
            (compile-expr-all (cons* `(
                                       (define tmp-var-modify ,expr)
                                       (drop var)
                                       (define ,var (destroy tmp-var-modify)))))
            ; same but copy the variable at the top first
            (compile-expr-all (cons* `(
                                       (roll var)
                                       (define tmp-var-modify ,expr)
                                       (drop var)
                                       (define ,var (destroy tmp-var-modify)))))))))
     ; copy
     ((list 'copy var) #:when (and (symbol? var) (member var stack))
                       (list (compile-var var)))
     ; contract
     ((list 'contract liste ...) (compile-contract-all liste))
     ((list 'public args expr ...) (compile-function e))
     ; verify expression
     ((list 'verify l)
      (define all-expr (compile-expr-all l))
      (for/list ((e all-expr)) (append e (list "OP_VERIFY"))))
     ; if ; todo
     ((list 'if cond true)
      (let* ((mycond (compile-expr-all cond))
             (mytrue (compile-expr-all true)))
        (for*/list ((c mycond) (t mytrue))
          (append
           c
           (list "OP_IF")
           t
           (list "OP_ENDIF")))))
     ((list 'if cond true false)
      (let* ((mycond (compile-expr-all cond))
             (mytrue (save-stack (compile-expr-all true)))
             (myfalse (compile-expr-all false)))
        (for*/list ((c mycond) (t mytrue) (f myfalse))
          (append
           c
           (list "OP_IF")
           t
           (list "OP_ELSE")
           f
           (list "OP_ENDIF")))))
     ; cons
     ((list 'cons a b) (let* ((all-a (compile-expr-all a))
                              (all-b (compile-expr-all b)))
                         (for*/list ((aa all-a)
                                     (bb all-b))
                         (begin ;(displayln a) (displayln bb) (displayln "-")
                                (append aa bb)))))
     ; debug
     ('debug (begin (println (format "STAAAAAAAAACK: ~a" stack)) '(())))
     ; ignore
     ((list 'ignore a) '(()))
     ; asm
     (e #:when (string? e) (list (list e)))
     ; nil
     (e '()))))
   ;(printf "FINN. Expr: ~a Stack: ~a~n" e stack)
  res)))

(define (toLEUnsigned n l)
  (define l++ `(+ ,l 1))
  (if (number? l) (set! l++ (+ l 1)) '())
  (compile-expr-all (cons* `((define tmp-m (call num2bin (,n ,l++)))
                             (define taille-tmp-m (call getLen (tmp-m)))
                             (bytes-get-first (destroy tmp-m) (- (destroy taille-tmp-m) 1))))))

(define (fromLEUnsigned bytes)
  (map (lambda (a) (append a '("OP_BIN2NUM"))) (compile-expr-all `(+bytes ,bytes "00"))))

(define (readVarint tx)
  ; il y avait un freeze-stack
  (compile-expr-all
   `(cons (define tx ,tx)
          (cons (define header (bytes-get-first tx 1))
                (if (= header "fd")
                    (cons (define l (call fromLEUnsigned ((bytes-delete-first (bytes-get-first tx 3) 1))))
                          (cons (drop header)
                                (bytes-get-first (bytes-delete-first (destroy tx) 3) (destroy l))))
                    (if (= header "fe")
                        (cons (define l (call fromLEUnsigned ((bytes-delete-first (bytes-get-first tx 5) 1))))
                              (cons (bytes-get-first (bytes-delete-first (destroy tx) 5) (destroy l))
                                    (drop header)))
                        (if (= header "ff")
                            (cons (define l (call fromLEUnsigned ((bytes-delete-first (bytes-get-first tx 9) 1))))
                                  (cons (bytes-get-first (bytes-delete-first (destroy tx) 9) (destroy l))
                                        (drop header)))
                            (cons (define l (call fromLEUnsigned ((destroy header))))
                                  (bytes-get-first (bytes-delete-first (destroy tx) 1) (destroy l))))))))))

(define (readVarint-compile-small tx)
  (begin
    ;(set! a-verifier (lambda (size) (< size 200)))
    (compile-expr-all
     `(cons (define tx ,tx)
            (cons (define header (bytes-get-first tx 1))
                  (cons (define l (call fromLEUnsigned ((destroy header))))
                        (bytes-get-first (bytes-delete-first (destroy tx) 1) (destroy l))))))))

; https://learnmeabitcoin.com/technical/varint
(define (writeVarint tx)
  (compile-expr-all
   `(cons (define txici ,tx)
          (cons (define n (call getLen (txici)))
                (if (<= n "fc00")
                    (+bytes (call toLEUnsigned ((destroy n) 1)) (destroy txici))
                    (if (<= n "ffff00")
                        (+bytes (+bytes "fd" (call toLEUnsigned ((destroy n) 2))) (destroy txici))
                        (if (<= n "ffffffff00")
                            (+bytes (+bytes "fe" (call toLEUnsigned ((destroy n) 4))) (destroy txici))
                            (+bytes (+bytes "ff" (call toLEUnsigned ((destroy n) 8))) (destroy txici))
                            )))))))

(define (writeVarint-compile-small tx)
  (compile-expr-all
   `(cons (define txici ,tx)
          (cons (define header (call getLen (txici))) ;size
                (+bytes (bytes-get-first (destroy header) 1) (destroy txici))))))

(define (getScriptCode tx)
  ;(car (compile-expr-all `(call readVarint-compile-small (bytes-delete-first ,(car tx) 104)))))
  (compile-expr-all `(call readVarint (bytes-delete-first ,(car tx) 104))))

(define (getScriptCode-with-header tx)
  (compile-expr-all `(bytes-get-first (bytes-delete-first ,(car tx) 104) "s1")))

(define (buildOutput args)
  ;(car (compile-expr-all `(+bytes ,(second args) (call writeVarint-compile-small (,(first args)))))))
  (compile-expr-all `(+bytes ,(second args) (call writeVarint (,(first args))))))

(define (buildOutput-with-header args)
  (compile-expr-all `(+bytes ,(second args) ,(first args))))

(define (hashOutputs args)
  (compile-expr-all `(cons (define abc ,(car args))
                                (bytes-get-last (bytes-delete-last (destroy abc) 8) 32))))

(define/contract (call-call function args)
  (-> symbol? list? (lambda (elem) (and (list? elem) (list? (first elem)))))
  ;(displayln function) (displayln args)
  (match function
    ('getLen (map (lambda (a) (append a '("OP_SIZE" "OP_NIP"))) (compile-expr-all (car args))))
    ('not (map (lambda (a) (append a '("OP_NOT"))) (compile-expr-all (car args))))
    ('invert (map (lambda (a) (append a '("OP_INVERT"))) (compile-expr-all (car args))))
    ('getScriptCode (getScriptCode args))
    ('getScriptCode-with-header (getScriptCode-with-header args))
    ('buildOutput (buildOutput args))
    ('buildOutputP2PKH (buildOutput (list `("OP_DUP" "OP_HASH160" "14" ,(first args) "OP_EQUALVERIFY" "OP_CHECKSIG") (second args)))) ; todo à tester (surement faux)
    ('buildOutput-with-header (buildOutput-with-header args))
    ;('buildOutput (list '("arg + call buildOutput")))
    ('hashOutputs (hashOutputs args))
    ;('hashOutputs (list '("arg + call hashOutputs")))
    ('readVarint (readVarint args))
    ('readVarint-compile-small (readVarint-compile-small args))
    ('writeVarint (writeVarint (car args)))
    ('writeVarint-compile-small (writeVarint-compile-small (car args)))
    ('fromLEUnsigned (fromLEUnsigned (car args)))
    ('toLEUnsigned (toLEUnsigned (first args) (second args)))
    ('bin2num (map (lambda (a) (append a '("OP_BIN2NUM"))) (compile-expr-all (car args))))
    ('num2bin (let ((all-f (compile-expr-all (first args))))
                (push-stack)
                (let ((all-s (compile-expr-all (second args))))
                  (drop-stack)
                  (for*/list ((f all-f) (s all-s))
                    (append
                     f
                     s
                     '("OP_NUM2BIN"))))))
    ('checksigverify (let ((all-f (compile-expr-all (first args))))
                       (push-stack)
                       (let ((all-s (compile-expr-all (second args))))
                         (drop-stack)
                         (for*/list ((f all-f) (s all-s))
                           (append
                            f
                            s
                            '("OP_CHECKSIGVERIFY"))))))
    ('hash256 (map (lambda (a) (append a '("OP_HASH256"))) (compile-expr-all (car args))))
    ('sha256 (map (lambda (a) (append a '("OP_SHA256"))) (compile-expr-all (car args))))
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
    ; not tested
    ('hashmap-exists (match-let (((list m_ key_ hint) args) ((list m key) (list (gensym) (gensym))))
                       (compile-expr-all (cons*
                                          `((define ,m ,m_)
                                            (define ,key ,key_)
                                            (define pos (* 64 ,hint))
                                            (define a (bytes-get-first (bytes-delete-first (destroy ,m) (destroy pos)) 32))
                                            (= (call sha256 ((destroy ,key))) (destroy a))
                                            )))))
    ; push tx. Tested, shouldn't change, but will be rewritten in lisp instead of assembly
    ('pushtx-assembly (save-stack (begin
                                    (push-stack)
                                    (compile-expr-all (cons* `(
                                                               "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220"
                                                               (define tx-arg ,(car args))
                                                               "OP_HASH256"
                                                               "OP_1ADD"
                                                               ;(modify my-tx-arg (+ 1 my-tx-arg)) ;should compile to 1ADD lmao OP_1 OP_OVER OP_ADD OP_SWAP OP_DROP
                                                               "OP_CAT"
                                                               "41" ;sighashflags
                                                               "OP_CAT"
                                                               "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0"
                                                               "OP_CHECKSIGVERIFY"
                                                               ;(drop tx-arg) checksigverify already drops the argument
                                                               ))))))
    ('pushtx-no-modify (compile-expr-all (cons* `(
                                                  (define temp-first-part "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220")
                                                  (define my-tx ,(car args)) ;copy tx arg
                                                  (define my-tx-hashed (call hash256 ((destroy my-tx))))
                                                  (define my-tx-hashed-plus1 (+ 1 (destroy my-tx-hashed)))
                                                  (define first-part (+bytes
                                                                      (destroy temp-first-part)
                                                                      (destroy my-tx-hashed-plus1)))
                                                  (define sighashflags "41")
                                                  (define signature (+bytes (destroy first-part) (destroy sighashflags)))
                                                  (define pubkey "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0")
                                                  (call checksigverify ((destroy signature) (destroy pubkey)))))))
    ('pushtx (compile-expr-all (cons* `(
                                        (define temp-first-part "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220")
                                        (define my-tx ,(car args)) ;copy tx arg
                                        (modify my-tx (call hash256 (my-tx)))
                                        (modify my-tx (+ 1 my-tx))
                                        (modify my-tx (+bytes (destroy temp-first-part) my-tx))
                                        (define sighashflags "41")
                                        (define signature (+bytes (destroy my-tx) (destroy sighashflags)))
                                        (define pubkey "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0")
                                        (call checksigverify ((destroy signature) (destroy pubkey)))))))
    ))

;(define (comp-save-all e) (save-stack (compile-expr-all e)))
; contract->ir = compile-expr-all
(define (ir->opcodes c)
  (string-join (filter (lambda (s) (or (= (string-length s) 1) (not (equal? "//" (substring s 0 2)))))
                                   (flatten (map to-opcode
                                                 (flatten (map (lambda (a) (string-split a " "))
                                                               (flatten c))))))))

(define (opcodes? c) (and (string? c) (> (string-length c) 2) (string=? (substring c 0 3) "OP_")))
(define (opcodes->size c) (foldl (lambda (e acc) (+ acc (if (opcodes? e) 1 (/ (string-length e) 2)))) 0 (string-split c " ")))

(define (contract->opcodes c (keep-args-order #t))
  (set-approx #f) (set-stack '()) ; make sure approx is off and stack is empty
  (define is-contract (equal? 'contract (first c)))
  (if is-contract
      (let ()
        (printf (~a "You are compiling a contract" (if keep-args-order "" " with a different order of args of public function") "\n"))
        (define res (first (compile-contract-all (cdr c) keep-args-order)))
        (define code-contract (naive-opt (ir->opcodes res)))
        (printf (~a "Size of the code: " (opcodes->size code-contract) "\n"))
        (printf "Code of the contract:\n")
        code-contract)
      ; else
      (let ()
        ; if it's a public function
        (define is-public-function (equal? 'public (first c)))
        ; change function used if keep-args-order AND list of args too big
        (if (and is-public-function (> (length (second c)) 4))
            (begin
              (println "Compilation without changing the number of arguments because number of args is too big")
              (set! compile-function compile-function-simple))
            '())
        (define allcomp (map naive-opt (map ir->opcodes (compile-expr-all c)))) ; ir->opcodes and naive opt
        (set! allcomp (map (lambda (a) (if (string=? (substring a 0 2) "||") (~a "this-function-has-no-args(çatodo) " a) a)) allcomp))
        (if is-public-function
            (let ()
              (printf (~a "You are compiling a public function, with" (if keep-args-order "out changing the order of your args" " any order of arguments") "\n"))
              (define opt-res (argmin opcodes->size allcomp))
              (define res (if keep-args-order (argmin opcodes->size (filter (lambda (c) (equal? (first (string-split c " || ")) (first (string-split (last allcomp) " || ")))) allcomp)) opt-res))
              (define code-function (second (string-split res " || ")))
              (printf (~a "Order of arguments: " (car (string-split res " || ")) "\n"))
              (if (not keep-args-order) (printf (~a "Original order: " (car (string-split (last allcomp) " || ")) "\n")) '())
              (printf (~a "Size of your function: " (opcodes->size code-function) (if keep-args-order (~a ", by allowing the changing order, the size would be " (opcodes->size (second (string-split opt-res " || ")))) "") "\n"))
              (printf "Code of the function\n")
              code-function)
            ; else: it's something else
            (argmin opcodes->size allcomp)))))

;(contract->opcodes '(public (a b) (+ (destroy a) (destroy b))))
;(contract->opcodes '(public (a b) (+ (destroy a) (destroy b))) #f)
;(contract->opcodes '(contract (public (a b c) (+ (destroy a) (destroy b)))))
;(contract->opcodes '(contract (public (a b c) (+ (destroy a) (destroy b)))) #f)

; Todo: fonction recursive qu'on appel avec un "niveau". profile-function était le 1er niveau ?
(define (subcontract->size c)
  (set-approx #t)
  (define allcomp (map naive-opt (map ir->opcodes (compile-expr-all c))))
  (set-approx #f)
  (opcodes->size (argmin opcodes->size allcomp)))

; Profile each line of the contract
(define (profile-function-simple contract)
  (if (equal? (first contract) 'public) '() (error "please give a public function")) ; fail if a public is not given
  ; set the stack with the argument of the public function
  (set! stack (second contract))
  (define code-contract (cddr contract))
  (define list-each-expr (for/list ((expr code-contract))
                           (map naive-opt (map ir->opcodes (compile-expr-all expr)))))
  (set! stack '()) ;reset the stack
  (define list-best (map (lambda (a) (argmin opcodes->size a)) list-each-expr))
  (printf "Result of the profiling (the orders of arguments may have been changed):~n")
  (for/list ((expr list-best) (code code-contract))
    (let ((size (opcodes->size expr)))
      (printf "~a~a opcodes ==> ~a~n" size (if (< size 100) (if (< size 10) "  " " ") "") code)))
  (printf "Sum of theses costs: ~a~n" (foldl + 0 (map opcodes->size list-best)))
  )

(define (profile-function contract)
  (if (equal? (first contract) 'public) '() (error "please give a public function")) ; fail if a public function is not given
  (set! stack (second contract))
  (define code-contract (filter (lambda (a) (not (and (list? a) (equal? 'ignore (first a))))) (cddr contract)))
  (recursive-profile-aux code-contract))

(define (recursive-profile-aux code (indent "") (father #f) (last-last-indent 0))
  ;(displayln code)
  ; code is a s-expr
  (define code-iter (if (and (list? code) (equal? 'call (first code))) (third code) code))
  ; if we are a cons
  (if (and (list? code) (equal? 'cons (first code))) (begin (set! indent "") (set! last-last-indent 0)) '())
  (define list-of-sexpr (for/list ((e code-iter)
                                   #:when (and
                                           ; exclude atomic expression and ignore statements
                                           (not (symbol? e))
                                           (not (number? e))
                                           (not (string? e))
                                           (not (and (list? e) (equal? 'ignore (first e))))))
                                   e))
  ; if it was a function call, the list of s-expr is the list of argument
  (define list-each-expr (save-stack (for/list ((expr list-of-sexpr)) (map naive-opt (map ir->opcodes (compile-expr-all expr))))))
  (define list-best (map (lambda (a) (argmin opcodes->size a)) list-each-expr))
  ;(displayln list-best)
  ;(printf "Result of the profiling (the orders of arguments may have been changed):~n")
  (for/list ((expr list-best) (c list-of-sexpr))
    (let* ((size (opcodes->size expr))
          (reverse-indent (make-string (- 15 (string-length indent)) #\=))
          ; find pos of c in father
          (child (~a c))
          (last-indent (+ last-last-indent (if father (or (for/last ((i (in-range (- (string-length father) (string-length child))))
                                                 #:when (string=? (substring father i (+ i (string-length child))) child))
                                        i)
                                      0)
                           0))))
      (printf "~a~a~a opcodes ~a=> ~a~a~n"
              indent
              size
              (if (< size 100) (if (< size 10) "  " " ") "")
              reverse-indent
              (make-string last-indent #\space)
              c)
      (recursive-profile-aux c (~a indent "  ") (~a c) last-indent)))
  ; apply stack modification
  (if (and (list? code) (member (first code) '(define destroy drop modify)))
      (compile-expr-all (cons (first code) ;define/destroy/drop/modify
                              (cons (second code) ;the var
                                    (if (member (first code) '(drop destroy)) '() (list 0))))) ;a random argument for define/modify
      '())
  )

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
    ; manually collect memory in ifs
    ((list 'if c t) '())
    ((list 'if c t f) '())
    ; recursion
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
            (append (reverse (collect-list-define code)) (reverse (second code)))
            is-public-function))

    

; export
(provide (all-defined-out))
