#lang racket

(require "../compilation.rkt")
(require "../vyper.rkt")


(define swap-state (vyper-create-final
                    '(a b c d e)
                    '((public ()
                              (define tmp a)
                              (modify a e)
                              (modify e tmp)
                              (drop tmp)
                              ))))

(opcodes->size (contract->opcodes swap-state))