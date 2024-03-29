#lang racket

;file not tested much

(require "../compilation.rkt")
(require "../hashtable.rkt")
(require "../vyper.rkt")

; test hashtable without the "stateful macro"
(contract->opcodes
                           `(contract
                             ; lookup
                             ,(unroll-addhint-final '(public
                                                       (tx amount map key) ; value hint) ;value puis hint
                                                       ;"OP_DROP" "OP_DROP" ;deux valeurs inutiles
                                                       ;"OP_DROP" ;index
                                                       (drop tx)
                                                       (drop amount)
                                                       (define cle key)
                                                       (define vv (hashmap-lookup (destroy map) (destroy key))); (call hashmap-lookup (map key value hint)))
                                                       ;(drop value) (drop hint)
                                                       ;(hashmap-lookup (destroy map) (destroy key)))
                                                       (= (+ 1 (destroy cle)) (destroy vv))
                                                       ;"OP_RETURN"
                                                       ))
                             ; add
                             ,(unroll-addhint-final '(public
                                                       (tx amount map key value)
                                                       (define scriptCode (call getScriptCode (tx)))
                                                       (define newmap (hashmap-add (destroy map) (destroy key) (destroy value)))
                                                       (define newAmount (call num2bin ((destroy amount) 8)))
                                                       (define newScriptCode (+bytes (bytes-delete-last (destroy scriptCode) 32) (call sha256 ((destroy newmap)))))
                                                       (define output (call buildOutput ((destroy newScriptCode) (destroy newAmount))))
                                                       (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx))))
                                                      ))
                             ; modify
                             ,(unroll-addhint-final '(public
                                                       (tx amount map key value)
                                                       (define scriptCode (call getScriptCode (tx)))
                                                       (define newmap (hashmap-modify (destroy map) (destroy key) (destroy value)))
                                                       (define newAmount (call num2bin ((destroy amount) 8)))
                                                       (define newScriptCode (+bytes (bytes-delete-last (destroy scriptCode) 32) (call sha256 ((destroy newmap)))))
                                                       (define output (call buildOutput ((destroy newScriptCode) (destroy newAmount))))
                                                       (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx))))
                                                      ))
                             ; delete
                             ,(unroll-addhint-final '(public
                                                       (tx amount map key value)
                                                       (define scriptCode (call getScriptCode (tx)))
                                                       (drop value)
                                                       (define newmap (hashmap-delete (destroy map) (destroy key))); 0))
                                                       (define newAmount (call num2bin ((destroy amount) 8)))
                                                       (define newScriptCode (+bytes (bytes-delete-last (destroy scriptCode) 32) (call sha256 ((destroy newmap)))))
                                                       (define output (call buildOutput ((destroy newScriptCode) (destroy newAmount))))
                                                       (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx))))
                                                      ))
                             ) #f)

(define cc (vyper-create-final
                    '(map)
'((public (key value |(hashmap-lookup_map_(destroy_key))_00| |(hashmap-lookup_map_(destroy_key))_01|)
          (verify
           (=
            (destroy value)
            (cons
             (ignore (add-hint 2 "(hashmap-lookup_map_(destroy_key))_0"))
             (call hashmap-lookup (map (destroy key) |(hashmap-lookup_map_(destroy_key))_00| |(hashmap-lookup_map_(destroy_key))_01|)))))
          (drop |(hashmap-lookup_map_(destroy_key))_00|)
          (drop |(hashmap-lookup_map_(destroy_key))_01|))
  (public (key value |(hashmap-add_map_(destroy_key)_(destroy_value))_00|)
          (modify
           map
           (cons (ignore (add-hint 1 "(hashmap-add_map_(destroy_key)_(destroy_value))_0")) (call hashmap-add (map (destroy key) (destroy value) |(hashmap-add_map_(destroy_key)_(destroy_value))_00|))))
          (drop |(hashmap-add_map_(destroy_key)_(destroy_value))_00|))
  (public (key value |(hashmap-modify_map_(destroy_key)_(destroy_value))_00|)
          (modify
           map
           (cons
            (ignore (add-hint 1 "(hashmap-modify_map_(destroy_key)_(destroy_value))_0"))
            (call hashmap-modify (map (destroy key) (destroy value) |(hashmap-modify_map_(destroy_key)_(destroy_value))_00|))))
          (drop |(hashmap-modify_map_(destroy_key)_(destroy_value))_00|))
  (public (key |(hashmap-delete_map_(destroy_key))_00|)
          (modify map (cons (ignore (add-hint 1 "(hashmap-delete_map_(destroy_key))_0")) (call hashmap-delete (map (destroy key) |(hashmap-delete_map_(destroy_key))_00|))))
          (drop |(hashmap-delete_map_(destroy_key))_00|)))))

(contract->opcodes (append '(public) '(()) cc))

; ok without pushtx
(contract->opcodes (append '(public) '(())
                                              (vyper-create-final
                                               '(map)
                                               (with-hashmap '(
                                                               (public (key value) (verify (= (destroy value) (hashmap-lookup map (destroy key)))))
                                                               (public (key value) (modify map (hashmap-add map (destroy key) (destroy value))))
                                                               (public (key value) (modify map (hashmap-modify map (destroy key) (destroy value))))
                                                               (public (key) (modify map (hashmap-delete map (destroy key))))
                                                               )))))


; name system
(contract->opcodes (vyper-create-final
                    '(name=>value ; map of name.bsv to whatever the user choose
                      name=>owner); map of name.bsv to an address that owns the name. Either the registar or the user
                    (with-hashmap '(
                                    ; register
                                    (public (name owner value)
                                            ;profit(fee)
                                            (modify name=>value (hashmap-add name=>value name (destroy value)))
                                            (modify name=>owner (hashmap-add name=>owner (destroy name) (destroy owner)))) ;requires a signature from the owner ?
                                    ; modify value
                                    (public (name newValue signature)
                                            (call checksigverify ((destroy signature) (hashmap-lookup name=>owner name)))
                                            (modify name=>value (hashmap-modify name=>value (destroy name) (destroy newValue))))
                                    ; modify owner
                                    (public (name newOwner signature)
                                            (define oldOwner (hashmap-lookup name=>owner name))
                                            (call checksigverify ((destroy signature) oldOwner)) ;verify a signature of the new owner ?
                                            (modify name=>owner (hashmap-modify name=>owner (destroy name) (destroy newOwner))))
                                    ))))
