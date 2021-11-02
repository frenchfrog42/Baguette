#lang racket


(require "../compilation.rkt")
(require "../hashtable.rkt")
(require "../util.rkt")

; useful in the created code by the foreach macro
(define (create-code-once function)
  (define code "")
  (new-var 'key)
  (new-var 'value)
  (set! code (first (compile-expr-all (function 'key 'value))))
  (delete-var 'key)
  (delete-var 'value)
  (displayln code)
  (contract->opcodes
   (list "40" "OP_SPLIT" "OP_SWAP" ;extract key+value
         "20" "OP_SPLIT" "OP_SWAP" ;extract key and value
         code))) ; code executed in the foreach loop

; foreach macro
(define-syntax-rule (build-foreach hashtable fonction n)
  (cons* (list
          `(modify hashtable hashtable)
          ; marker to know where the foreach loop is
          "codeici"
          ; code unrolled of the foreach loop
          (let ((code-n-time (make-list n code-once))) ; must burn key and value
            code-n-time)
          ; assert map is empty at the end
          "OP_0" "OP_EQUALVERIFY")))

(define code-once 0)
(define hexa 0)
(define taille-liste 0)

; The foreach contract
(define (test-foreach) `(contract
                         ; This public function will execute a foreach loop
                         (public (hashmap)
                                 ; foreach loop. Not very coherent but for now it destroys the map
                                 (foreach hashmap
                                          ,(lambda (key value) `(verify (= (destroy ,key) (destroy ,value)))))
                                 ; end of the function. In a real contract you'd check next output or whatever
                                 1)
                         ; Changing the hashmap. This will change the foreach loop above
                         (public (newHashtable oldHashtable tx-arg amount-arg new-codeforeach)
                                 ;(define size (/ (call getLen ((destroy newHashtable))) 64))
                                 (drop newHashtable)
                                 ; verify new-codeforeach is size * code-once
                                 (define scriptCode (call getScriptCode (tx-arg)))
                                 (define size-old (/ (call getLen ((destroy oldHashtable))) 64))
                                 (define scriptCode_ ,(+bytes* `(
                                                                 (bytes-get-first scriptCode "r1")
                                                                 (destroy new-codeforeach)
                                                                 (bytes-delete-first (destroy scriptCode) (+ (* (destroy size-old) ,taille-liste) "r1")))))
                                 (define newAmount (call num2bin ((destroy amount-arg) 8)))
                                 (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                                 (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg)))))))


; Once the contract is compiled, we replace "r1" by the position of the foreach loop in the code. OP_8 in this example
(define (replace-index-foreach str)
  (define liste (string-split str))
  (define valeur (~a "OP_" (index-of liste "codeici"))) ;marche que si l'index est petit, sinon todo
  (displayln valeur)
  (define a-replace (indexes-of liste "r1"))
  (string-join (remove "codeici" (list-set (list-set liste (first a-replace) valeur) (second a-replace) valeur))))

; Recursively search for "foreach"
(define (recursive-search-replace-foreach l n)
  (define this (lambda (l) (recursive-search-replace-foreach l n)))
  (match l
    ((list 'foreach map fonction) (build-foreach ,map ,fonction n))
    ((cons a b) (cons (this a) (this b)))
    (_ l)))
; Recursively search for the function of the foreach loop
(define (recursive-search-foreach l)
  (define this recursive-search-foreach)
  (match l
    ((list 'foreach map fonction) fonction)
    ((cons a b) (or (this a) (this b)))
    (_ #f)))

; Build one contract
(define (build-one-contract contract n)
  (recursive-search-replace-foreach contract n))

; Build all contracts
(for/list ((n '(0 1 2 3 4 5)))
  (set! code-once (create-code-once (recursive-search-foreach (test-foreach))))
  (set! hexa (string-join (map opcode->hex (string-split code-once)) ""))
  (set! taille-liste (/ (string-length hexa) 2))
  (define contract (test-foreach))
  (display-to-file #:exists 'replace
                   (~a
                    (file->string "template.json")
                    (replace-index-foreach (contract->opcodes (build-one-contract contract n)))
                    "\"}")
                   (~a "test_foreach_" n ".json")))