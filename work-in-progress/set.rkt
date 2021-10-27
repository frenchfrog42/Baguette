#lang racket
(require file/sha1)
(require "../compilation.rkt")
(require "../util.rkt")

(bytes->hex-string
 (sha256-bytes
  (hex-string->bytes "0a")))

(define depth 2)

(define contract
  ; fails if name already taken
  ; update the root of the tree
  `(public
     ; arguments ==> string value hint0-left hint0-right hint1-left ... to hint,(depth - 1)-right
     ,(append '(string value old-root)
              (flatten (for/list ((i (in-range depth)))
                         (list
                          (string->symbol (~a "hint" i "-left"))
                          (string->symbol (~a "hint" i "-right"))))))
     ; code of the function
     (define current-hash (call sha256 (value))) ;for debug
     (define new-root (call sha256 (1))) ;the new root starts from 1
     ,(cons*
       (for/list ((i (in-range depth)))
         (cons* `(
                  ; first verify hints
                  (define current-byte (bytes-get-first (bytes-delete-first value ,i) 1))
                  ; hint0-left should be of size size(curren-byte)
                  (verify (= (call getLen ((destroy current-byte))) (call getLen (hint0-left)))); -1 maybe
                  ; modify current-hash
                  ,(let ((left (string->symbol (~a "hint" i "-left")))
                         (right (string->symbol (~a "hint" i "-right"))))
                     `(cons
                       (modify current-hash (call sha256 (,(+bytes* `(,left current-hash ,right))))) ;for debug
                       (modify new-root (call sha256 (,(+bytes* `(,left new-root ,right)))))))))))
     ; now, the name was free iif value was 0 (otherwise the value is 1)
     ; so iif the new-root is different from the old one
     (verify (call not ((= new-root old-root))))
     ; update new root
     ))

(contract->opcodes contract)