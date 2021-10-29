#lang racket
(require file/sha1)
(require "../compilation.rkt")
(require "../util.rkt")

;(bytes->hex-string (sha256-bytes (hex-string->bytes ""))) ; OP_0 OP_SHA256

(define depth 9)
(define how-many-per-level 16)

; verification of the choosen depth 
(let* (
       ; a-z A-Z 0-9 ==> 2*26 + 10 = 62
       (size-of-string 62)
       ; what length do we want
       (length-of-string 1)
       ; so, how many strings possible ?
       ; all sequences of size <= n (so from size = 0, to size = length-of-strings, not (- length 1))
       (how-many-strings (apply + (for/list ((i (in-range (+ length-of-string 1)))) (expt size-of-string i))))
       ; now, bytes
       ; 256 bytes possible
       (size-of-bytes 255) ; 256 ou 255 ?
       ; how-many different bytes
       (how-many-bytes (expt size-of-bytes depth)) ;maybe -1 ? todo
       ; how many if we took (- depth 1)
       (just-below (expt size-of-bytes (- depth 1))))
  ; verification checks
  (if (< how-many-bytes how-many-strings)
      (error "Not enough depth :/") '())
  (if (> just-below how-many-strings)
      (println "Depth is too high !") '()))


; todo voir le reverse de la liste des arguments
; voir comment on va deploy tout ça
; faire tous les checks dans le contract, et construire le state

(define contract
  ; fails if name already taken
  ; update the root of the tree
  `(public
     ; arguments ==> string value hint0-left hint0-right hint1-left ... to hint,(depth - 1)-right
     ,(append '(string) ;real-new-root for debug
              (flatten (for/list ((i (in-range depth)))
                         (list
                          (string->symbol (~a "hint" i "-left-first"))
                          (string->symbol (~a "hint" i "-right-first"))
                          (string->symbol (~a "hint" i "-left-second"))
                          (string->symbol (~a "hint" i "-right-second"))))))
     ; code of the function
     (define new-root (call sha256 (1))) ; the new root starts from 1. More efficient to call sha256 on 1, then replacing it by the value lol
     ,(cons*
       (for/list ((i (in-range depth)))
         (let ((left1 (string->symbol (~a "hint" i "-left-first")))
               (right1 (string->symbol (~a "hint" i "-right-first")))
               (left2 (string->symbol (~a "hint" i "-left-second")))
               (right2 (string->symbol (~a "hint" i "-right-second"))))
           (cons* `(
                    ; après tester n'importe quoi au hasard c'est la config qui donne la taille minimale, wtf
                    (define current-byte (bytes-get-first (bytes-delete-first string ,i) 1)) ;todo
                    ; first "half byte"
                    (define first-part (call bin2num ((& current-byte "0f"))))
                    ; hint-left-first should be of size size(first-part), and same for hint-left-second
                    (verify (= (destroy first-part) (/ (call getLen (,left1)) 32))) ;-1 ?
                    (define second-part (call bin2num ((>> (& (destroy current-byte) "f0") 4))))
                    (verify (= (destroy second-part) (/ (call getLen (,left2)) 32))) ;-1 ?
                    (modify new-root (call sha256 (,(+bytes* `((destroy ,left1) new-root (destroy ,right1))))))
                    (modify new-root (call sha256 (,(+bytes* `((destroy ,left2) new-root (destroy ,right2)))))))))))
     ; verification of hints
     ;(define real-old-root 0) ;debug
     ;(verify (= real-old-root old-root)) ;ok ;real-old-root is root parsed from state
     ;(verify (= real-new-root new-root))
     (destroy new-root)
     ; right now, everything is verified, we can do the computation

     ; update new root to the state
     ;todo
     ))

(define first-root
  (let ((tmp (sha256-bytes (hex-string->bytes "")))) ;we start from OP_0
    (for/list ((i (in-range depth)))
      (set! tmp (sha256-bytes (list->bytes (flatten (for/list ((_ (in-range how-many-per-level))) (bytes->list tmp)))))))
    (bytes->hex-string tmp)))

(define un (sha256-bytes (hex-string->bytes "")))
(define lun (bytes->list un))
(bytes->hex-string un)

(define deux (sha256-bytes (list->bytes (flatten (for/list ((d (in-range how-many-per-level))) lun)))))
(define ldeux (bytes->list deux))
(bytes->hex-string deux)

(define trois (sha256-bytes (list->bytes (flatten (for/list ((d (in-range how-many-per-level))) ldeux)))))
(bytes->hex-string trois)
; first hint
;(bytes->hex-string (list->bytes (flatten (for/list ((_ (in-range (- how-many-per-level 1)))) lun))))

(ignore (bytes->hex-string
 (sha256-bytes (list->bytes (append
                             (flatten (for/list ((_ (in-range (- how-many-per-level 1)))) (bytes->list (sha256-bytes (hex-string->bytes "")))))
                             (bytes->list (sha256-bytes (hex-string->bytes "01"))))))))

first-root

(contract->opcodes contract)



;todo article

; for a set, we check value was something different from 0, the new value is 1
; to do an hashmap with this, we'd do (verify value-of-string) to check if the element is here
; we would return value-of-string and check (= new-root old-root) if we want to do a lookup
; we would just update the root, without any check on value-of-string for the javascript "set"
; and set (at the start of new-root) to either hash(0), or hash(something else) for a deletion/modification