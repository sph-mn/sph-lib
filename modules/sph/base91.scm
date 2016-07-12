; (sph base91) - base91 encoding/decoding
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
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
  (export
    base91-decode
    base91-encode)
  (import
    (rnrs arithmetic bitwise)
    (rnrs base)
    (rnrs bytevectors)
    (rnrs hashtables)
    (sph)
    (only (guile) modulo reverse!)
    (only (sph vector) vector-each-with-index))

  (define chars-encode
    (vector #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N
      #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d
      #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
      #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\! #\# #\$ #\% #\& #\( #\) #\* #\+ #\, #\. #\/ #\: #\; #\< #\= #\>
      #\? #\@ #\[ #\] #\^ #\_ #\` #\{ #\| #\} #\~ #\"))

  (define chars-decode
    (let (r (make-eqv-hashtable 91))
      (vector-each-with-index (l (e index) (hashtable-set! r e index)) chars-encode) r))

  (define (base91-decode a) "string -> bytevector"
    (u8-list->bytevector
      (reverse!
        (let (a-length (string-length a))
          (let next ((index 0) (bits 0) (shift 0) (value #f) (r (list)))
            (if (< index a-length)
              (let (index-decode (hashtable-ref chars-decode (string-ref a index) #f))
                (if index-decode
                  (if value
                    (let*
                      ( (value (+ value (* index-decode 91)))
                        (bits (bitwise-ior bits (bitwise-arithmetic-shift-left value shift)))
                        (shift (+ shift (if (> (bitwise-and value 8191) 88) 13 14))))
                      (let next-2
                        ( (bits (bitwise-arithmetic-shift-right bits 8)) (shift (- shift 8))
                          (r (pair (bitwise-and bits 255) r)))
                        (if (> shift 7)
                          (next-2 (bitwise-arithmetic-shift-right bits 8) (- shift 8)
                            (pair (bitwise-and bits 255) r))
                          (next (+ 1 index) bits shift #f r))))
                    (next (+ 1 index) bits shift index-decode r))
                  (next (+ 1 index) bits shift value r)))
              (if value
                (pair
                  (bitwise-and (bitwise-arithmetic-shift-left (bitwise-ior bits value) shift) 255)
                  r)
                r)))))))

  (define (base91-encode a) "bytevector -> string"
    (list->string
      (reverse!
        (let (a-length (bytevector-length a))
          (let next ((index 0) (shift 0) (bits 0) (r (list)))
            (if (< index a-length)
              (let
                (bits
                  (bitwise-ior bits
                    (bitwise-arithmetic-shift-left (bytevector-u8-ref a index) shift)))
                (if (> (+ 8 shift) 13)
                  (let (value (bitwise-and bits 8191))
                    (if (> value 88)
                      (next (+ 1 index) (- shift 5)
                        (bitwise-arithmetic-shift-right bits 13)
                        (pairs (vector-ref chars-encode (truncate (/ value 91)))
                          (vector-ref chars-encode (modulo value 91)) r))
                      (next (+ 1 index) (- shift 6)
                        (bitwise-arithmetic-shift-right bits 14)
                        (let (value (bitwise-and bits 16383))
                          (pairs (vector-ref chars-encode (truncate (/ value 91)))
                            (vector-ref chars-encode (modulo value 91)) r)))))
                  (next (+ 1 index) (+ 8 shift) bits r)))
              (if (> shift 0)
                ( (l (r)
                    (if (or (> shift 7) (> bits 90))
                      (pair (vector-ref chars-encode (truncate (/ bits 91))) r) r))
                  (pair (vector-ref chars-encode (modulo bits 91)) r))
                r))))))))
