#lang racket
(require file/sha1)
(require "../compilation.rkt")
(require "../util.rkt")

;(bytes->hex-string (sha256-bytes (hex-string->bytes ""))) ; OP_0 OP_SHA256

(define depth 9)
(define how-many-per-level 16)

;a-z A-Z 0-9 ==> 2*26 + 10 = 62
;one byte => 256

;3bytes => 16777216 (256**3)
;length4 > 14776336 (62**4)

; size of depth9 = 650 in size (401 without old root)
; and it eq to length 12

(define contract
  ; fails if name already taken
  ; update the root of the tree
  `(public
     ; arguments ==> string value hint0-left hint0-right hint1-left ... to hint,(depth - 1)-right
     ,(append '(string value-of-string real-old-root real-new-root)
              (flatten (for/list ((i (in-range depth)))
                         (list
                          (string->symbol (~a "hint" i "-left-first"))
                          (string->symbol (~a "hint" i "-right-first"))
                          (string->symbol (~a "hint" i "-left-second"))
                          (string->symbol (~a "hint" i "-right-second"))))))
     ; code of the function
     (define old-root (call sha256 (0))) ; computation of the old-root
     (define new-root (call sha256 (value-of-string))) ; the new root starts from 1 (or something else (todo))
     ,(cons*
       (for/list ((i (in-range depth)))
         (cons* `(
                  ; first verify hints
                  (define current-byte (bytes-get-first (bytes-delete-first string ,i) 1))
                  (define first-part (call bin2num ((& current-byte "0f"))))
                  (define second-part (call bin2num ((>> (& current-byte "f0") 4))))
                  ; hint0-left should be of size size(curren-byte)
                  (drop current-byte); (verify (= ((destroy current-byte)) (/ (call getLen (hint0-left)) 32))); -1 maybe ; mouais verif ça
                  ; modify current-hash
                  ,(let ((left1 (string->symbol (~a "hint" i "-left-first")))
                         (right1 (string->symbol (~a "hint" i "-right-first")))
                         (left2 (string->symbol (~a "hint" i "-left-second")))
                         (right2 (string->symbol (~a "hint" i "-right-second"))))
                     (cons* `(
                              (modify old-root (call sha256 (,(+bytes* `(,left1 old-root ,right1)))))
                              (modify new-root (call sha256 (,(+bytes* `((destroy ,left1) new-root (destroy ,right1))))))
                              (modify old-root (call sha256 (,(+bytes* `(,left2 old-root ,right2)))))
                              (modify new-root (call sha256 (,(+bytes* `((destroy ,left2) new-root (destroy ,right2)))))))))))))
     ; verification of hints
     ;(define real-old-root 0) ;debug
     (verify (= real-old-root old-root)) ;ok ;real-old-root is root parsed from state
     (verify (= real-new-root new-root))
     (drop old-root) (drop real-old-root) (drop string) (drop new-root) (drop real-new-root);debug
     ; right now, everything is verified, we can do the computation

     ; for a set, we check value was something different from 0, the new value is 1
     ; to do an hashmap with this, we'd do (verify value-of-string) to check if the element is here
     ; we would return value-of-string and check (= new-root old-root) if we want to do a lookup
     ; we would just update the root, without any check on value-of-string for the javascript "set"
     ; and set (at the start of new-root) to either hash(0), or hash(something else) for a deletion/modification
     (verify value-of-string)
     (destroy value-of-string) ;debug
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