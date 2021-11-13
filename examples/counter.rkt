#lang racket

(require "../compilation.rkt")
(require "../vyper.rkt")
(require "../util.rkt")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter with manual memory management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compteur '(public (tx-arg amount-arg)
                          (call pushtx (tx-arg))
                          (define scriptCode (call getScriptCode (tx-arg)))
                          (define counter (call bin2num ((bytes-get-last scriptCode 1))))
                          (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode) 1) (+ 1 (destroy counter))))
                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                    ))

(contract->opcodes compteur) ; Fonctionne. 293bytes
(profile-function-simple compteur)
(profile-function compteur)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter with garbage collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compteur-nomemory
  (garbage-collector (remove-destroy-variable compteur) #f))

(contract->opcodes compteur-nomemory) ; Fonctionne. 308bytes
(profile-function-simple compteur-nomemory) ;different because args order is changed
(profile-function compteur-nomemory)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter with state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compteur-state (vyper-create-final '(compteur)
                                            '((public () (modify compteur (+ 1 compteur))))))

(contract->opcodes compteur-state) ; Fonctionne. 313 bytes




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter with experimental features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compteur-opt '(public (tx-arg amount-arg)
                              ;(call pushtx (tx-arg))
                              (define scriptCode-with-header (call getScriptCode-with-header (tx-arg)))
                              (define counter (call bin2num ((bytes-get-last scriptCode-with-header 1))))
                              (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode-with-header) 1) (+ 1 (destroy counter))))
                              (define newAmount (call num2bin ((destroy amount-arg) 8)))
                              (define output (call buildOutput-with-header ((destroy scriptCode_) (destroy newAmount))))
                              (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                              ))

(define size (length (string-split (contract->opcodes compteur-opt))))
(define (replace-compteur-opt code) (string-replace code "s1" (first (compile-int (+
                                                                                   size
                                                                                   2 ;op return and state
                                                                                   1 ;s1 count for 2
                                                                                   4 ;pouet
                                                                                   )))))
(replace-compteur-opt (contract->opcodes compteur-opt)) ; Fonctionne. 49bytes

(profile-function-simple compteur-opt)
(profile-function compteur-opt)