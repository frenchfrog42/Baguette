#lang racket
(require "../compilation.rkt")

(define pushtx '(public (tx-arg)
                        ; assrt my-tx-arg is at the top of the stack
                        "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220"
                        ; should be written in lisp, not assembly
                        "OP_OVER"
                        "OP_HASH256"
                        "OP_1ADD"
                        ;(modify my-tx-arg (+ 1 my-tx-arg)) ;should compile to 1ADD lmao OP_1 OP_OVER OP_ADD OP_SWAP OP_DROP
                        "OP_CAT"
                        "41" ;sighashflags
                        "OP_CAT"
                        "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0"
                        "OP_CHECKSIGVERIFY"
                        (drop tx-arg)
                        1))

(define compteur '(public (tx-arg amount-arg)
                          (call pushtx (tx-arg))
                          (define scriptCode (call getScriptCode (tx-arg)))
                          (define counter (call bin2num ((bytes-get-last scriptCode 1))))
                          (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode) 1) (+ 1 (destroy counter))))
                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                    ))

(contract->opcodes compteur) ;works