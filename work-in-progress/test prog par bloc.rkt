#lang racket

; bool (int n) {
;   res = 0;
;   for (int i = 1; i < n; i++)
;     res *= i;
;   res == 7;
; }

; chaque bloc compile indépendamment
; transition entre chaque bloc
; déclaration des variables au début pour avoir le state qui est supposé pas changer pendant l'execution

; étapes
; ecrire le morph
; pouvoir calculer le hash d'un contrat (wtf) ou alors demander cette info au programmeur
; compiler (cons a b) en b, a(hash b), et recursivement avec le reste
; faire un état * où on peut sortir de l'automate


; démo
; deux vars. a et b. Des ints de taille 1

(define-syntax-rule (whileloop-block iter while-cond hash-script-code)
                     `(public (tx-arg amount-arg next-script-code)
                              ; oppushtx tx-arg
                              (define scriptCode (call getScriptCode (tx-arg)))
                              ; ensure next script code is the right one to morph
                              (verify (= ,hash-script-code (call hash256 (next-script-code))))
                              ; state
                              (define tmp (bytes-get-last scriptCode 2))
                              (define a (call bin2num (bytes-get-first tmp 1)))
                              (define b (call bin2num (bytes-get-last tmp 1)))
                              ,(iter 'a 'b)
                              (drop tmp)
                              (drop a)
                              (drop b)
                              ; (assert-exists new_a)
                              ; (assert-exists new_b)
                              ; computing outputs
                              (define newAmount (call num2bin ((destroy amount-arg) 8)))
                              ; first output: continue to loop
                              (define scriptCode_1 (+bytes (bytes-delete-last scriptCode 2)
                                                           (+bytes new_a new_b)))
                              (define output_1 (call buildOutput ((destroy scriptCode_1) newAmount)))
                              ; second output: exit the loop
                              (define scriptCode_2 (+bytes (destroy next-script-code) (+bytes "6a02" (bytes-get-last scriptCode 2))))
                              (define output_2 (call buildOutput ((destroy scriptCode_2) newAmount)))
                              ; drops
                              (drop newAmount)
                              (drop scriptCode)
                              ; the end
                              (define hashoutputs (call hashOutputs ((destroy tx-arg))))
                              (define res_1 (= (call hash256 ((destroy output_1))) hashoutputs))
                              (define res_2 (= (call hash256 ((destroy output_2))) hashoutputs))
                              (drop hashoutputs)
                              (if ,(while-cond 'new_a 'new_b)
                                  (cons (drop res_2) (cons (drop new_a) (cons (drop new_b) (destroy res_1)))) ; if true -> continue
                                  (cons (drop res_1) (cons (drop new_a) (cons (drop new_b) (destroy res_2)))) ; else    -> exit
                              )))

(whileloop-block
 ; iter
 (lambda (a b) `(cons (define my-tmp ,a)
                      (cons (define new_a (+ ,a ,b))
                            (cons (define new_b my-tmp) (drop my-tmp)))))
 ; exit
 (lambda (a b) `(< ,a 10))
 "hash")


(define-syntax-rule (verify-block verify-line hash-script-code)
                     `(public (tx-arg amount-arg next-script-code)
                              ; oppushtx tx-arg
                              (define scriptCode (call getScriptCode (tx-arg)))
                              ; ensure next script code is the right one to morph
                              (verify (= ,hash-script-code (call hash256 (next-script-code))))
                              ; state
                              (define tmp (bytes-get-last scriptCode 2))
                              (define a (call bin2num (bytes-get-first tmp 1)))
                              (define b (call bin2num (bytes-get-last tmp 1)))
                              ,(verify-line 'a 'b)
                              (drop tmp)
                              (drop a)
                              (drop b)
                              ; computing outputs
                              (define newAmount (call num2bin ((destroy amount-arg) 8)))
                              (define scriptCode (+bytes (destroy next-script-code) (+bytes "6a02" (bytes-get-last scriptCode 2))))
                              (define output (call buildOutput ((destroy scriptCode_2) newAmount)))
                              ; drops
                              (drop newAmount)
                              (drop scriptCode)
                              ; the end
                              (define hashoutputs (call hashOutputs ((destroy tx-arg))))
                              (= (call hash256 ((destroy output))) hashoutputs)
                              ))

(verify-block
 (lambda (a b) `(verify (call not (= ,a ,b))))
 "hash")


(define-syntax-rule (return-block end-result)
                     `(public (tx-arg)
                              ; oppushtx tx-arg
                              (define scriptCode (call getScriptCode (destroy tx-arg)))
                              (define tmp (bytes-get-last (destroy scriptCode) 2))
                              (define a (call bin2num (bytes-get-first tmp 1)))
                              (define b (call bin2num (bytes-get-last tmp 1)))
                              (define result ,(end-result 'a 'b))
                              (drop tmp)
                              (drop a)
                              (drop b)
                              result
                              ))

(return-block
 (lambda (a b) `(& (= ,a 13) (= ,b 8))))

; a = 1
; b = 1

; while (a < 10) {
;   tmp = a
;   a = a + b
;   b = tmp
; }

; verify (a != b)

; return (= a 13)

; needs to be instanciated with the correct hash
(define morph-only-to '(public (tx-arg amount-arg next-script-code)
                         ; oppushtx tx-arg
                         (define scriptCode (call getScriptCode (tx-arg)))
                         ; ensure next script code is the right one to morph
                         (define hash-script-code 2);todo
                         (verify (= (destroy hash-script-code) (call hash256 (next-script-code))))
                         ; morph to the new code
                         (define scriptCode_ (+bytes (destroy next-script-code) (+bytes "6a02" (bytes-get-last (destroy scriptCode) 2))))
                         (define newAmount (call num2bin ((destroy amount-arg) 8)))
                         (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                         (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                    ))


(define (compiled-to-opcode c)
  (begin ;(print c)
  (foldl (lambda (a b) (format "~a ~a" b a)) ""
         (naive-opt
          (filter (lambda s (not (string=? "//" (substring (car s) 0 2))))
                  (flatten (for/list ((e (flatten c))) (map to-opcode (string-split e " ")))))))))

(define (getsize c) (length (string-split (compiled-to-opcode c) " ")))

(define (best-version e)
  (define comp-e (compile-expr-all e))
  (define all-size (map getsize comp-e))
  (define mini (argmin getsize comp-e))
  (define final-version (compiled-to-opcode mini))
  (printf ". List of size: ~a~n. Best size: ~a~n. Final version: ~a~n" all-size (foldl min (first all-size) (cdr all-size)) final-version))

(define l (compile-expr-all compteur))
(compiled-to-opcode (car l))
(length l)

; (best-version compteur-opt)

; todo taille variable
(ignore (progn
(define (getcode1 tx)
  `(bytes-delete-last (bytes-delete-first tx ,(+ 104 1)) 52))
(define (getcode2 tx)
  `(bytes-delete-last (bytes-delete-first tx ,(+ 104 1 2)) 52))

(compile-expr-all '(public (tx) (call getScriptCode (tx))))


(compile-expr-all '(public (tx) (if (+ 1 2) 2 3)))

))

(define tmp-table '(                    (a 1)                    (b 1)                    (c 2)))
;(define tmp-table (quote ((a 4) (b 4))))
(define table
  (let ((acc-begin 1) (acc-end 1))
    (for/list ((e tmp-table)) (begin
                                (set! acc-end acc-begin)
                                (set! acc-begin (+ acc-begin (cadr e)))
                                (list (car e) acc-end acc-begin)
                                ))))

(define size-state (last (last table)))


(define-syntax-rule (test-solidity user-args user-code) ;rajouter les args
  `(public ,(append '(tx-arg amount-arg) user-args)
           ; oppushtx tx-arg
           (define scriptCode (call getScriptCode (tx-arg)))
           ; get state
           (define tmp (bytes-get-last scriptCode ,size-state))
           ,(cons* (for/list ((e table)) `(define ,(car e) (call bin2num (bytes-get-last (bytes-get-first tmp ,(caddr e)) ,(- (caddr e) (cadr e)))))))
           (drop tmp)
           ; list of outputs
           (define output 0) ; empty list at the begin => OP_0
           ; USER CODE
           ,(cons* 'user-code)
           ; computing the last output, which is the smart contract
           (define newAmount (call num2bin ((destroy amount-arg) 8)))
           (define scriptCode (+bytes (destroy next-script-code) (+bytes "6a" (+bytes ,size-state ,(+bytes* (for/list ((e table)) (list 'call 'num2bin `(destroy ,(car e)) (- (caddr e) (cadr e)))))))))
           (modify output (+bytes output (call buildOutput ((destroy scriptCode) (destroy newAmount)))))
           ; the end
           (define hashoutputs (call hashOutputs ((destroy tx-arg))))
           (= (call hash256 ((destroy output))) hashoutputs)
           ))

(test-solidity '(x) (
                 (define abc (+ a b))
                 (define abcd abc)))

(test-solidity '() ((cons (define tmp (+ a 1)) (cons (drop a) (define a (destroy tmp)))) (cons (define tmp (+ b 1)) (cons (drop b) (define b (destroy tmp))))))


(define pub '(contract
              (public ()
                     (define x 2)
                     (modify x (+ 1 2))
                     (= x 1))
              (public ()
                     (= 1 1))))

(best-version pub)
