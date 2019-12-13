; (sph base91) - base91 encoding/decoding
; written with information from: http://base91.sourceforge.net/.
; Copyright (C) 2010-2019 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph base91)
  (export base91-decode base91-encode sph-base91-description)
  (import (rnrs base) (rnrs arithmetic bitwise) (rnrs bytevectors) (only (rnrs r5rs) modulo))

  (define sph-base91-description "encoder/decoder")

  (define chars-encode
    (vector #\A #\B
      #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R
      #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h
      #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x
      #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\! #\# #\$ #\%
      #\& #\( #\) #\* #\+ #\, #\. #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\] #\^ #\_ #\` #\{ #\| #\} #\~ #\"))

  (define chars-decode
    (let loop ((index 0) (result (make-vector 256 #f)))
      (if (< index (vector-length chars-encode))
        (begin (vector-set! result (char->integer (vector-ref chars-encode index)) index)
          (loop (+ 1 index) result))
        result)))

  (define (base91-decode a) "string -> bytevector"
    ; results collected on the stack with nested function applications.
    ; bitwise-and to truncate to eight bits
    (u8-list->bytevector
      (let ((a-length (string-length a)))
        (let next ((index 0) (value #f) (bits 0) (shift 0))
          (if (< index a-length)
            (let ((decode (vector-ref chars-decode (char->integer (string-ref a index)))))
              (if decode
                (if value
                  (let*
                    ( (value (+ value (* decode 91)))
                      (bits (bitwise-ior bits (bitwise-arithmetic-shift-left value shift)))
                      (shift (+ shift (if (> (bitwise-and value 8191) 88) 13 14))))
                    (cons (bitwise-and bits 255)
                      (let next-sub
                        ((bits (bitwise-arithmetic-shift-right bits 8)) (shift (- shift 8)))
                        (if (> shift 7)
                          (cons (bitwise-and bits 255)
                            (next-sub (bitwise-arithmetic-shift-right bits 8) (- shift 8)))
                          (next (+ 1 index) #f bits shift)))))
                  (next (+ 1 index) decode bits shift))
                (next (+ 1 index) value bits shift)))
            (if value
              (list
                (bitwise-ior (bitwise-and bits 255) (bitwise-arithmetic-shift-left value shift)))
              (list)))))))

  (define (base91-encode a) "bytevector -> string"
    (list->string
      (let ((a-length (bytevector-length a)))
        (let next ((index 0) (bits 0) (shift 0))
          (if (< index a-length)
            (let
              ( (bits
                  (bitwise-ior bits
                    (bitwise-arithmetic-shift-left (bytevector-u8-ref a index) shift)))
                (shift (+ 8 shift)))
              (if (> shift 13)
                (let ((value (bitwise-and bits 8191)))
                  (if (> value 88)
                    (cons (vector-ref chars-encode (modulo value 91))
                      (cons (vector-ref chars-encode (truncate (/ value 91)))
                        (next (+ 1 index) (bitwise-arithmetic-shift-right bits 13) (- shift 13))))
                    (let ((value (bitwise-and bits 16383)))
                      (cons (vector-ref chars-encode (modulo value 91))
                        (cons (vector-ref chars-encode (truncate (/ value 91)))
                          (next (+ 1 index) (bitwise-arithmetic-shift-right bits 14) (- shift 14)))))))
                (next (+ 1 index) bits shift)))
            (if (< 0 shift)
              (cons (vector-ref chars-encode (modulo bits 91))
                (if (or (< 7 shift) (< 90 bits))
                  (list (vector-ref chars-encode (truncate (/ bits 91)))) (list)))
              (list))))))))
