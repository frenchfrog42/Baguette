#lang racket


(require "../compilation.rkt")
(require "../hashtable.rkt")
(require "../util.rkt")

; todo mettre ça dans util
(define (opcode->hex a)
  (match a
    ("OP_1" "51")
    ("OP_2" "52")
    ("20" "0120")
    ("40" "0140")
    ("OP_SPLIT" "7f")
    ("OP_SWAP" "7c")
    ("OP_BIN2NUM" "81")
    ("OP_PICK" "79")
    ("OP_EQUAL" "87")
    ("OP_VERIFY" "69")
    ("OP_1ADD" "8b")))

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
          code))) ; verify key == value for each element of the map


(define (test-fonction a b) `(verify (= (destroy ,a) (destroy ,b))))

(define code-once (create-code-once test-fonction))
(define hexa (string-join (map opcode->hex (string-split code-once)) ""))
(define taille-liste (/ (string-length hexa) 2))

(define (verif-hashtable n) `(contract
                              ; foreach code
                              (public (hashtable); code-foreach)
                                ; place hashtable at the top of the stack
                                (modify hashtable hashtable)
                                ; code foreach
                                "codeici"
                                ,(let ((code-n-time (make-list n code-once))) ; must burn key and value
                                   code-n-time)
                                ; assert map is empty
                                "OP_0" "OP_EQUALVERIFY"
                                ; end of the function. Check next output ?
                                1)
                              ; changing the hashmap
                              (public (newHashtable oldHashtable tx-arg amount-arg new-codeforeach)
                                ;(define size (/ (call getLen ((destroy newHashtable))) 64))
                                (drop newHashtable)
                                ; verify new-codeforeach is size * code-once
                                (define scriptCode (call getScriptCode (tx-arg)))
                                ; todo voir ça
                                ; ordre des args la dans le template = pas comme les arguments maintenant
                                ;"OP_FALSE" "OP_VERIFY"
                                ;(verify (= (call getLen (new-codeforeach)) ,(* taille_liste (+ n 1))))
                                ; (verify (= new-codeforeach (bytes-delete-first (bytes-get-first scriptCode (+ ,(* n taille-liste) 8)) 8))); ok
                                (define size-old (/ (call getLen ((destroy oldHashtable))) 64))
                                (define scriptCode_ ,(+bytes* `(
                                                                (bytes-get-first scriptCode "r1")
                                                                (destroy new-codeforeach)
                                                                (bytes-delete-first scriptCode (+ (* (destroy size-old) ,taille-liste) "r1")))))
                                (drop scriptCode)
                                (define newAmount (call num2bin ((destroy amount-arg) 8)))
                                (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                                (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg)))))))

(define (replace-r1 str)
  (define liste (string-split str))
  (define valeur (~a "OP_" (index-of liste "codeici"))) ;marche que si l'index est petit, sinon todo
  (displayln valeur)
  (define a-replace (indexes-of liste "r1"))
  (string-join (remove "codeici" (list-set (list-set liste (first a-replace) valeur) (second a-replace) valeur))))
  
; 'public (a b) -> a au fond

(verif-hashtable 2)
(contract->opcodes (verif-hashtable 2))
(replace-r1 (contract->opcodes (verif-hashtable 2)))

(for/list ((n '(0 1 2 3 4 5)))
  (display-to-file #:exists 'replace
   (~a
    (file->string "template.json")
    (replace-r1 (contract->opcodes (verif-hashtable n)))
    "\"}")
   (~a "test_foreach_" n ".json")))