#lang racket

(require "../compilation.rkt")
(require "../vyper.rkt")

(define compteur '(public (tx-arg amount-arg)
                          (define scriptCode (call getScriptCode (tx-arg)))
                          (define counter (call bin2num (bytes-get-last scriptCode 1)))
                          (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode) 1) (+ 1 (destroy counter))))
                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                    ))

(contract->opcodes compteur) ; Fonctionne

(contract->opcodes
 (garbage-collector
  (remove-destroy-variable compteur) #f)) ;Fonctionne

(define compteur-opt '(public (tx-arg amount-arg)
                              (define scriptCode-with-header (call getScriptCode-with-header (tx-arg)))
                              (define counter (call bin2num (bytes-get-last scriptCode-with-header 1)))
                              (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode-with-header) 1) (+ 1 (destroy counter))))
                              (define newAmount (call num2bin ((destroy amount-arg) 8)))
                              (define output (call buildOutput-with-header ((destroy scriptCode_) (destroy newAmount))))
                              (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                              ))

(contract->opcodes compteur-opt #f) ; Dans le code faut remplacer s1 par 3d. Fonctionne. 3d = 59


(define compteur-state (vyper-create-final '(compteur)
                                            '((public () (modify compteur (+ 1 compteur))))))

(contract->opcodes compteur-state) ; Fonctionne

(+ 1
   (+ 2 3)
   (+ 4 5)
        )

(subcontract->size '(+ a a))
