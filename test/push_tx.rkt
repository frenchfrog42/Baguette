#lang racket
(require rackunit)
(require "../compilation.rkt")

(define handcoded
  '(
    ; assert tx-arg at the top of the stack
    "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220"
    "OP_OVER"
    "OP_HASH256"
    "OP_1ADD"
    ;(modify my-tx-arg (+ 1 my-tx-arg)) ;should compile to 1ADD lmao OP_1 OP_OVER OP_ADD OP_SWAP OP_DROP
    "OP_CAT"
    "41" ;sighashflags
    "OP_CAT"
    "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0"
    "OP_CHECKSIGVERIFY"
    ;(drop tx-arg) checksigverify already drops the argument
    ))

(define lispcoded-no-modify '(
                    (define temp-first-part "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220")
                    (define my-tx tx-arg) ;copy tx arg
                    (define my-tx-hashed (call hash256 ((destroy my-tx))))
                    (define my-tx-hashed-plus1 (+ 1 (destroy my-tx-hashed)))
                    (define first-part (+bytes
                                        (destroy temp-first-part)
                                        (destroy my-tx-hashed-plus1)))
                    (define sighashflags "41")
                    (define signature (+bytes (destroy first-part) (destroy sighashflags)))
                    (define pubkey "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0")
                    (call checksigverify ((destroy signature) (destroy pubkey)))))

(define lispcoded '(
                    (define temp-first-part "3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220")
                    (define my-tx tx-arg) ;copy tx arg
                    (modify my-tx (call hash256 (my-tx)))
                    (modify my-tx (+ 1 my-tx))
                    (modify my-tx (+bytes (destroy temp-first-part) my-tx))
                    (define sighashflags "41")
                    (define signature (+bytes (destroy my-tx) (destroy sighashflags)))
                    (define pubkey "02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0")
                    (call checksigverify ((destroy signature) (destroy pubkey)))))

; todo faire un appel a pushtx

(if (string=?
     (contract->opcodes (append '(public (tx-arg)) lispcoded-no-modify))
     (contract->opcodes (append '(public (tx-arg)) lispcoded)))
    '() (exit 1))
(if (string=?
     (contract->opcodes (append '(public (tx-arg)) lispcoded))
     (contract->opcodes (append '(public (tx-arg)) handcoded)))
    '() (exit 1))