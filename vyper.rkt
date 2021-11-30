#lang racket

(require "compilation.rkt")
(require "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stateful contract
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Le contrat sera de la forme:
; Stack d'entrée: plein de merde | index | elements propre à chaque fonction
; Vérification que les éléments données sont corrects
; Chaque fonction publique
; Ces fonctions publique prennent comme liste d'argument "ça". Elle peut pas changer (seulement la deuxième partie peut changer)



; prends une liste d'argument qui doit être la de toute façon, et les ajoute à une fonction publique
(define (vyper-create-public-function args-deja-present fonction-publique)
  (append
   (list 'public (append args-deja-present (cadr fonction-publique))) ;args
   (cddr fonction-publique))) ;function code

; test de la fonction d'avant
;(car (compile-expr-all (vyper-create-public-function '(a b) '(public (c) (destroy c) (destroy b)))))

; prends une liste de fonction publique, et construit le contrat avec state
(define (vyper-create-final liste-state liste-fonction)
  (printf "~a~n~a~n" liste-state liste-fonction)
  (define liste-données '(amount-arg tx-arg)) ; tx-arg doit etre a la fin ; la liste des données qu'on aura dans chaque fonction publique, etc...
  (define reponse '()) ; le code final
  
  ; on place au fond l'index qui est au top
  (define taille-données-state (+ (length liste-données) (length liste-state)))
  (set! reponse (append (for/list ((_ (in-range taille-données-state))) (~a "roll" taille-données-state))))

  ; Au début la stack est de la forme:           bullshit     | liste-state | index | arguments de la fonction
  ; On veut la transformer en:         liste-données | tx-arg | liste-state | index | arguments de la fonction

  ;todo

  ; On bouge tout ça sur l'altstack
  (set! reponse (append reponse (for/list ((e (in-range (- (length liste-données) 1)))) "OP_TOALTSTACK")))
  (set! reponse (append reponse (list "OP_DUP" "OP_TOALTSTACK")))

  
  ; Maintenant la stack est de la forme:  tx-arg | liste-state | index | arguments de la fonction
  ; On veut la transformer en:                     liste-state | index | arguments de la fonction
  ; C'est l'étape de vérification de liste-state

  ; On pushtx, et on récupère dans le state de la transaction la concatenation hashée de tous les éléments
  ; push tx
  (set-stack '(tx-arg))
  (set! reponse (append reponse (first (compile-expr-all '(call pushtx (tx-arg))))))
  ; verify state
  (set-stack '(tx-arg))
  (set! reponse (append reponse (car (compile-expr-all (cons* `(
                                                                (define scriptCode (call getScriptCode ((destroy tx-arg))))
                                                                (bytes-get-last (destroy scriptCode) 32))))))) ; la liste concaténé des éléments du state
  (set-stack '())
  ; La stack est de la forme: hash | liste-state | ...
  (set! reponse (append reponse (list "OP_TOALTSTACK" "OP_0"))) ; On la bouge sur l'altstack, et on push un accumulateur (une liste de bytes vide)
  (set! reponse (append reponse (flatten (for/list ((idx (in-range (length liste-state))))
                                           (define res '())
                                           (set! res (append res (list (string-join (list "pick" (~a (+ 1 idx))) "")))) ; On pick l'elem
                                           (set! res (append res (list "OP_SHA256" "OP_CAT"))) ; On hash puis on concatene. Il faut hasher sinon 0102|03 est pareil que 01|0203
                                           res))))
  (set! reponse (append reponse (list "OP_SHA256" "OP_FROMALTSTACK" "OP_EQUALVERIFY"))) ; On la ramene sur la stack, et on compare les hashs. On hash pour avoir taille constante mais inutile en vrai
  ; La concatenation est a + b + c, si la liste du state est '(a b c)

  
  ; La stack est maintenant: liste-state | index | arguments de la fonction
  
  ; On converti pour le bon type pour chaque élément du state (?)
  ;(set! reponse (append reponse (flatten (for/list ((e liste-state)) (list (string-join (list "roll" (format "~a" (- (length liste-state) 1))) "") "OP_BIN2NUM")))))
  
  ; Maintenant on compile la liste des fonctions
  ; On place l'index au top, et on selectionne la fonction. L'index va être drop au début de la fonction
  (set! reponse (append reponse (list (string-join (list "roll" (format "~a" (length liste-state))) "")))) ; l'index est enfoui sous len(state)

  ; La liste des fonctions publiques
  (set! reponse
        (append reponse
                
                                (flatten (for/list ((fonction liste-fonction) (i (in-range (length liste-fonction))))
                                  (begin ;(displayln expr)
                                    (set-stack (append (list 'output) liste-state))
                                         (append
                                          (car (compile-expr-all (list '= "pick0" i)))
                                          (list "OP_IF" "drop0")
                                          ; push the output variable
                                          (list "OP_0")
                                          ; place arguments of public function at the top. Il y a output | liste-state | args-fonction
                                          (for/list ((var-fonction (second fonction))) (format "roll~a" (+ (- (length (second fonction)) 1) (length liste-state) 1)))
                                          ; todo: voir si faut mettre first ou last (à priori first)
                                          ; todo: ajouter a fonction les arguments liste-données et liste-state
                                          ;todo first ou last
                                          ; c'est last, sinon ça inverse l'ordre des arguments de la fonction publique
                                          (flatten (cdr (last (compile-function-all (append
                                                                                       fonction
                                                                                       ; push output at the top
                                                                                       (list '(modify output output))
                                                                                       ; push liste-state at the top. Stack is liste-state | output
                                                                                       (for/list ((var (reverse liste-state))) `(define ,(format "state-~a" var) (destroy ,var))))))))
                                          (list "OP_ELSE")))))
                                (list "OP_FALSE OP_VERIFY") ; todo faire un truc qui fail ? En 1 opcode ? Avec l'index ?
                                (for/list ((i (in-range (length liste-fonction)))) "OP_ENDIF")))

  ; La stack est maintenant new-liste-state | output
  ; On concatene pour avoir le nouveau state
  (set! reponse (append reponse (list "OP_0") (flatten (for/list ((e liste-state)) (list "OP_SWAP OP_SHA256" "OP_CAT")))))
  (set! reponse (append reponse (list "OP_SHA256")))
  ; On récupère liste-données de l'altstack
  (set! reponse (append reponse (for/list ((e liste-données)) "OP_FROMALTSTACK")))

  ; La stack est maintenant: liste-données | new-hash-state | output


  ; Maintenant qu'on a toutes les fonctions publiques on construit l'output
  ; Les fonctions publiques sont sensées purger tout leur args, et nous donner tous les arguments du state
  (set-stack (append liste-données (list 'state 'output)))
  (set! reponse (append reponse (car (compile-expr-all (cons* `(
                                                                (define scriptCode (call getScriptCode (tx-arg)))
                                                                (define new-scriptCode (+bytes (bytes-delete-last (destroy scriptCode) 32) (destroy state))) ; la liste concaténé des éléments du state
                                                                ; computing the last output, which is the smart contract
                                                                (define newAmount (call num2bin ((destroy amount-arg) 8)))
                                                                (modify output (+bytes output (call buildOutput ((destroy new-scriptCode) (destroy newAmount))))) ; add the smart contract to the list of output
                                                                ; the end
                                                                (define hashoutputs (call hashOutputs ((destroy tx-arg))))
                                                                (= (call hash256 ((destroy output))) (destroy hashoutputs))
                                                                ))))))
  reponse)



; export
(provide (all-defined-out))




;(vyper-create-final '(a b)
;                    '((public (d) (modify a (destroy d)))
;                      (public (d) (modify b (destroy d)))
;                      ))

