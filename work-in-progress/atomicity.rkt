#lang racket
(require "../compilation.rkt")
(require "../util.rkt")
(require "../vyper.rkt")
(require file/sha1)

;todo in util ?
(define (sha256h h)
  (bytes->hex-string (sha256-bytes (hex-string->bytes h))))


(define contract
  (vyper-create-final '(a b c)
                      '((public (value) (modify a (destroy value)))
                        (public (value) (modify b (destroy value)))
                        (public (value) (modify c (destroy value))))))

;(contract->opcodes contract)

(define liste-of-function (list
                           '(public (value c b a output) (modify a (destroy value)))
                           '(public (value c b a output) (modify b (destroy value)))
                           '(public (value c b a output) (modify c (destroy value)))))

(define liste-of-code (map (lambda (a) (string-join (map opcode->hex (string-split (contract->opcodes a) " ")) "")) liste-of-function))


(define result (map sha256h liste-of-code))

(define sorted-result '(
                        "b1251189c54b80e7862a1c7c913a3d7663f2a47d3616f8d8b8eda76e744b37ea"
                        "c8cb9947615bea9f8b30764c4b5492a3f427cee846695c6286178981901ca5aa"
                        "227d4bbf6574c18c7124e3af5651c945f04fca546ac5a7f3bbac5364466145a2"))

(define hashmap-set (string-join (for/list ((key sorted-result)) (~a key (sha256h ""))) ""))

(define n 3)
(define list-of-args (for/list ((i (in-range n))) (string->symbol (~a "arg" i))))
(define hashmap-exists '+bytes)
(define magical-function `(public
                            ; args
                            ,list-of-args
                            ; code
                            ; verify each function is part of the map
                            (define map ,hashmap-set)
                            ,(cons* (for/list ((a list-of-args)) `(verify (,hashmap-exists map ,a))))
                            ; prepare the next output (parse state, etc...). Todo later
                            (define scriptCode "")
                            ; concatenate each function to form the next script code
                            (modify scriptCode (+bytes scriptCode ,(+bytes* list-of-args)))
                            ; ensure the next output is the previous function
                            ; todo later
                            ))

(contract->opcodes magical-function)