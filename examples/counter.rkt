#lang racket

(require "../compilation.rkt")
(require "../vyper.rkt")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter with manual memory management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compteur '(public (tx-arg amount-arg)
                          (define scriptCode (call getScriptCode (tx-arg)))
                          (define counter (call bin2num (bytes-get-last scriptCode 1)))
                          (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode) 1) (+ 1 (destroy counter))))
                          (define newAmount (call num2bin ((destroy amount-arg) 8)))
                          (define output (call buildOutput ((destroy scriptCode_) (destroy newAmount))))
                          (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                    ))

(contract->opcodes compteur) ; Fonctionne. 174bytes
(profile-function compteur) ;174bytes




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Counter with garbage collection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compteur-nomemory
  (garbage-collector (remove-destroy-variable compteur) #f))

(contract->opcodes compteur-nomemory) ; Fonctionne. 189bytes
(profile-function compteur-nomemory) ;188, because args changed, (contract->opcodes compteur-nomemory #f) = 188




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
                              (define scriptCode-with-header (call getScriptCode-with-header (tx-arg)))
                              (define counter (call bin2num (bytes-get-last scriptCode-with-header 1)))
                              (define scriptCode_ (+bytes (bytes-delete-last (destroy scriptCode-with-header) 1) (+ 1 (destroy counter))))
                              (define newAmount (call num2bin ((destroy amount-arg) 8)))
                              (define output (call buildOutput-with-header ((destroy scriptCode_) (destroy newAmount))))
                              (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                              ))

; In the script code you need to replace s1 with 3d (which is 59). Todo write a code that does this
(contract->opcodes compteur-opt #f) ; Fonctionne. 50bytes
(profile-function compteur-opt)