#lang racket

; work in progress for foreach on hashmaps

(define (opcode-to-hex a)
  (match a
    ("OP_1" "51")
    ("OP_2" "52")
    ("OP_SPLIT" "7f")
    ("OP_SWAP" "7c")
    ("OP_BIN2NUM" "81")
    ("OP_PICK" "79")
    ("OP_EQUAL" "87")
    ("OP_VERIFY" "69")
    ("OP_1ADD" "8b")))

(define (build-foreach l)
  (define comp (compiled-to-opcode (car (compile-expr-all (cons* l)))))
  (define list-of-opcode (string-split comp " "))
  (define taille (length list-of-opcode))
  (define list-hex-code (map opcode-to-hex list-of-opcode))
  ; returns
  (define hex-code (string-join list-hex-code ""))
  (define indice-debut 10)
  (define indice-fin (+ indice-debut taille))
  (define code-opcode (substring comp 1))
  (values indice-debut indice-fin hex-code code-opcode))

(build-foreach
 (list
  ; stack:: tab i
  1 "OP_SPLIT OP_SWAP OP_BIN2NUM"
  ; stack:: elem tab' i
  2 "OP_PICK"
  ; stack:: i elem tab' i
  "OP_EQUAL OP_VERIFY"
  ; stack:: tab' i
  "OP_SWAP OP_1ADD OP_SWAP"
  ; stack:: tab' i'
  ))

(let ((indice-debut-foreach 99)
      (indice-fin-foreach 99)
      (code-inside-foreach "aaaa")
      (code-opcodes "OP_RETURN")
      (taille-actuelle-tableau 2))
  (define foreach `(public (tx-arg amount-arg tab codeforeach elem quelle-fonction)
                           ; get state
                           (define scriptCode (call getScriptCode (tx-arg)))
                           (define hash-tab (bytes-get-last scriptCode 32))
                           (verify (= (call hash256 (tab)) hash-tab))

                         
                           ; first public function
                           (modify quelle-fonction quelle-fonction)
                           (modify tab tab)
                           ; stack:: tab quelle-fonction
                           "OP_OVER" ; quelle-fonction will be dropped after if/then/else
                           "OP_IF"
                         
                           ; change state
                           ;(modify tab (+bytes tab elem))
                           (+bytes tab elem)
                           "OP_SWAP OP_DROP"


                           ; second public function
                           "OP_ELSE"
                         
                           ; foreach (e: tab) { require(e == i); i++; }
                           "OP_0" ;(define i 0)
                           "OP_SWAP"
                           ; stack:: tab i
                           (ignore ,(let-values (((a b c d) (build-foreach
                                                             (list
                                                              ; stack:: tab i
                                                              1 "OP_SPLIT OP_SWAP OP_BIN2NUM"
                                                              ; stack:: elem tab' i
                                                              2 "OP_PICK"
                                                              ; stack:: i elem tab' i
                                                              "OP_EQUAL OP_VERIFY"
                                                              ; stack:: tab' i
                                                              "OP_SWAP OP_1ADD OP_SWAP"
                                                              ; stack:: tab' i+1
                                                              ))))
                                      (set! indice-debut-foreach a)
                                      (set! indice-fin-foreach b)
                                      (set! code-inside-foreach c)
                                      (set! code-opcodes d)
                                      ))
                           ; génération du code de la boucle
                           "OP_NOP"
                           ,(string-join (build-list taille-actuelle-tableau (lambda (a) code-opcodes)))
                           "OP_NOP"
                           ; stack:: tab i
                           "OP_SWAP OP_DROP" ;(drop i)
                           ; fin du code de la boucle
                           "OP_ENDIF"
                           (drop elem)
                           (drop quelle-fonction)
                         
                           ; computing the output
                           (define newAmount (call num2bin ((destroy amount-arg) 8)))
                           (define newScriptCode
                             ,(+bytes* `((bytes-get-first scriptCode ,indice-debut-foreach)
                                         codeforeach
                                         (bytes-delete-last (bytes-delete-first scriptCode ,indice-fin-foreach) 32)
                                         (destroy hash-tab))))
                           ; on vérifie qu'on a le bon code de boucle
                           ;(verify (call repetition codeforeach ,code-inside-foreach))
                           (drop tab)
                           (drop codeforeach)
                           ; fin des drops à la place du verify qui va destroy (?)
                           (drop scriptCode)
                           (define output (call buildOutput ((destroy newScriptCode) (destroy newAmount))))
                           (= (call hash256 ((destroy output))) (call hashOutputs ((destroy tx-arg))))
                           ))
  '()) ;(best-version foreach))

