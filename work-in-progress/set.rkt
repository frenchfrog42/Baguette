#lang racket
(require file/sha1)
(require "../compilation.rkt")
(require "../util.rkt")

;(bytes->hex-string (sha256-bytes (hex-string->bytes ""))) ; OP_0 OP_SHA256

(define depth 1)

(define contract
  ; fails if name already taken
  ; update the root of the tree
  `(public
     ; arguments ==> string value hint0-left hint0-right hint1-left ... to hint,(depth - 1)-right
     ,(append '(string value-of-string old-root)
              (flatten (for/list ((i (in-range depth)))
                         (list
                          (string->symbol (~a "hint" i "-left"))
                          (string->symbol (~a "hint" i "-right"))))))
     ; code of the function
     (define old-root (call sha256 (value-of-string))) ; computation of the old-root
     (define new-root (call sha256 (1))) ; the new root starts from 1 (or something else (todo))
     ,(cons*
       (for/list ((i (in-range depth)))
         (cons* `(
                  ; first verify hints
                  (define current-byte (bytes-get-first (bytes-delete-first value-of-string ,i) 1))
                  ; hint0-left should be of size size(curren-byte)
                  (verify (= (call getLen ((destroy current-byte))) (call getLen (hint0-left)))); -1 maybe
                  ; modify current-hash
                  ,(let ((left (string->symbol (~a "hint" i "-left")))
                         (right (string->symbol (~a "hint" i "-right"))))
                     `(cons
                       (modify old-root (call sha256 (,(+bytes* `(,left old-root ,right)))))
                       (modify new-root (call sha256 (,(+bytes* `(,left new-root ,right))))))))))) ;todo drop left/right
     ; verification of hints
     (define real-old-root 0) ;debug
     (verify (= real-old-root old-root)) ;real-old-root is root parsed from state
     ; right now, everything is verified, we can do the computation

     ; for a set, we check value was something different from 0, the new value is 1
     ; to do an hashmap with this, we'd do (verify value-of-string) to check if the element is here
     ; we would return value-of-string and check (= new-root old-root) if we want to do a lookup
     ; we would just update the root, without any check on value-of-string for the javascript "set"
     ; and set (at the start of new-root) to either hash(0), or hash(something else) for a deletion/modification
     (verify value-of-string)
     
     ; update new root to the state
     ;todo
     ))

(define first-root
  (let ((tmp (sha256-bytes (hex-string->bytes "")))) ;we start from OP_0
    (for/list ((i (in-range depth)))
      (set! tmp (sha256-bytes (list->bytes (flatten (for/list ((_ (in-range depth))) (bytes->list tmp)))))))
    (bytes->hex-string tmp)))

(define un (sha256-bytes (hex-string->bytes "")))
(define lun (bytes->list un))
(bytes->hex-string un)

(define deux (sha256-bytes (list->bytes (flatten (for/list ((d (in-range depth))) lun)))))
(define ldeux (bytes->list deux))
(bytes->hex-string deux)

(define trois (sha256-bytes (list->bytes (flatten (for/list ((d (in-range depth))) ldeux)))))
(bytes->hex-string trois)

first-root

;(contract->opcodes contract)