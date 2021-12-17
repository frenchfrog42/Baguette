#lang racket

(define licence "
/**
Copyright (c) 2021 Matter Web Services Inc., Attila Aros, Baguette
 
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
 
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
Addtionally, the ASM Versions are included in this copyright:
A)
<assetId (36 bytes)> <pkh 20 bytes> OP_2 OP_PICK OP_HASH160 OP_OVER OP_EQUALVERIFY OP_2OVER OP_CHECKSIGVERIFY OP_4 OP_PICK OP_NOTIF OP_6 OP_PICK OP_2 OP_PICK 0 24 OP_NUM2BIN OP_EQUAL OP_IF OP_8 OP_PICK 68 OP_SPLIT OP_DROP 44 OP_SPLIT OP_NIP OP_ELSE OP_2 OP_PICK OP_ENDIF OP_CAT OP_6 OP_PICK OP_CAT OP_8 OP_PICK 69 OP_10 OP_PICK 69 OP_SPLIT OP_DROP 68 OP_SPLIT OP_NIP 00 OP_CAT OP_BIN2NUM OP_ADD OP_SPLIT OP_DROP a300 OP_SPLIT OP_NIP OP_CAT OP_HASH256 OP_8 OP_PICK OP_9 OP_PICK OP_SIZE OP_NIP OP_8 OP_SUB OP_SPLIT OP_DROP OP_9 OP_PICK OP_SIZE OP_NIP 28 OP_SUB OP_SPLIT OP_NIP OP_EQUALVERIFY OP_ENDIF OP_7 OP_PICK OP_HASH256 OP_1 OP_SPLIT OP_SWAP OP_BIN2NUM OP_1ADD OP_SWAP OP_CAT 3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220 OP_SWAP OP_CAT <sighashType> OP_CAT 02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0 OP_CHECKSIG OP_NIP OP_NIP OP_NIP OP_NIP OP_NIP OP_NIP OP_NIP OP_NIP
B)
<assetId (36 bytes)> <pkh 20 bytes> OP_2 OP_PICK OP_HASH160 OP_OVER OP_EQUALVERIFY OP_2OVER OP_CHECKSIGVERIFY OP_4 OP_PICK OP_NOTIF OP_6 OP_PICK OP_2 OP_PICK 0 24 OP_NUM2BIN OP_EQUAL OP_IF OP_8 OP_PICK 68 OP_SPLIT OP_DROP 44 OP_SPLIT OP_NIP OP_ELSE OP_2 OP_PICK OP_ENDIF OP_CAT OP_6 OP_PICK OP_CAT OP_8 OP_PICK 69 OP_10 OP_PICK 69 OP_SPLIT OP_DROP 68 OP_SPLIT OP_NIP 00 OP_CAT OP_BIN2NUM OP_ADD OP_SPLIT OP_DROP a300 OP_SPLIT OP_NIP OP_CAT OP_HASH256 OP_8 OP_PICK OP_9 OP_PICK OP_SIZE OP_NIP OP_8 OP_SUB OP_SPLIT OP_DROP OP_9 OP_PICK OP_SIZE OP_NIP 28 OP_SUB OP_SPLIT OP_NIP OP_EQUALVERIFY OP_ENDIF OP_7 OP_PICK OP_HASH256 OP_1 OP_SPLIT OP_SWAP OP_BIN2NUM OP_1ADD OP_SWAP OP_CAT 3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220 OP_SWAP OP_CAT <sighashType> OP_CAT 02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0 OP_CHECKSIG OP_TOALTSTACK OP_2DROP OP_2DROP OP_2DROP OP_2DROP OP_FROMALTSTACK
C)
<assetId (36 bytes)> <pkh 20 bytes> OP_2 OP_PICK OP_HASH160 OP_SWAP OP_EQUALVERIFY OP_ROT OP_ROT OP_CHECKSIGVERIFY OP_DUP OP_0 24 OP_NUM2BIN OP_EQUAL OP_IF OP_DROP OP_3 OP_PICK 44 OP_SPLIT OP_NIP 24 OP_SPLIT OP_DROP OP_ENDIF OP_3 OP_ROLL OP_SWAP OP_3 OP_ROLL OP_CAT OP_CAT OP_2 OP_PICK 68 OP_SPLIT OP_NIP OP_1 OP_SPLIT OP_DROP 00 OP_CAT OP_BIN2NUM 69 OP_ADD OP_3 OP_PICK OP_SWAP OP_SPLIT OP_DROP a300 OP_SPLIT OP_NIP OP_CAT OP_SWAP OP_NOTIF OP_DUP OP_HASH256 OP_2 OP_PICK OP_SIZE 28 OP_SUB OP_SPLIT OP_NIP 20 OP_SPLIT OP_DROP OP_EQUALVERIFY OP_ENDIF 3044022079be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980220 OP_ROT OP_HASH256 OP_1 OP_SPLIT OP_SWAP OP_BIN2NUM OP_1ADD OP_SWAP OP_CAT OP_CAT <sighashType> OP_CAT 02b405d7f0322a89d0f9f3a98e6f938fdc1c969a8d1382a2bf66a71ae74a1e83b0 OP_CHECKSIGVERIFY
*/
")

(require "../compilation.rkt")
(require "../vyper.rkt")
(require "../hashtable.rkt")
(require "../util.rkt")

(define assetId "$assetid")
(define pubKeyHash "$pubKeyHash")

(define contract
  `(public (txPreimage outputSatsWithSize receiveAddressWithSize isTransform senderSig unlockKey)
           (define this-assetId-tmp ,assetId)
           (define this-pubKeyHash ,pubKeyHash)
           (verify (= (call hash160 (unlockKey)) (destroy this-pubKeyHash)))
           (call checksigverify ((destroy senderSig) (destroy unlockKey)))
           
           (define this-assetId (destroy this-assetId-tmp))
           (if (= this-assetId (call num2bin (0 36)))
               (cons (drop this-assetId)
                     (bytes-get-first (bytes-delete-first txPreimage 68) ,(- 104 68)))
               (destroy this-assetId))
           (unsafe-define tmp)
           (define accumulator ,(+bytes* `((destroy outputSatsWithSize) (destroy tmp) (destroy receiveAddressWithSize))))
           
           (define elem (bytes-get-first (bytes-delete-first txPreimage 104) 1))
           (modify elem (+bytes elem "00"))
           (modify elem (call bin2num (elem)))
           (modify elem (+ 105 elem))
           (bytes-delete-first (bytes-get-first txPreimage (destroy elem)) "a300") ;todo, 163
           (unsafe-define tmp)
           (modify accumulator (+bytes accumulator (destroy tmp)))

           (if (call not ((destroy isTransform)))
               (verify (=
                        (call hash256 (accumulator))
                        (bytes-get-first (bytes-get-last txPreimage 40) 32))))
           (call pushtx-assembly-scrypt ((destroy txPreimage)))
           ))

(profile-function contract)
(contract->opcodes contract)