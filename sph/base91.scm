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
    (only (guile) reverse!)
    (only (rnrs lists) cons*)
    (only (rnrs r5rs) modulo)
    (only (sph vector) vector-each-with-index))

  (define chars-encode
    (vector #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N
      #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d
      #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
      #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
      #\! #\# #\$ #\% #\& #\( #\) #\* #\+ #\. #\/ #\: #\; #\< #\= #\>
      #\? #\@ #\[ #\] #\^ #\_ #\` #\{ #\| #\} #\~ #\"))

  (define chars-decode
    (let (res (make-eq-hashtable 91))
      (vector-each-with-index (l (ele index) (hashtable-set! res ele index)) chars-encode) res))

  (define (base91-decode arg) "string -> bytevector"
    (u8-list->bytevector
      (reverse!
        (let (arg-length (string-length arg))
          (let next ((index 0) (bits 0) (shift 0) (value #f) (res (list)))
            (if (< index arg-length)
              (let (index-decode (hashtable-ref chars-decode (string-ref arg index) #f))
                (if index-decode
                  (if value
                    (let*
                      ( (value (+ value (* index-decode 91)))
                        (bits (bitwise-ior bits (bitwise-arithmetic-shift-left value shift)))
                        (shift (+ shift (if (> (bitwise-and value 8191) 88) 13 14))))
                      (let next-2
                        ( (bits (bitwise-arithmetic-shift-right bits 8)) (shift (- shift 8))
                          (res (cons (bitwise-and bits 255) res)))
                        (if (> shift 7)
                          (next-2 (bitwise-arithmetic-shift-right bits 8) (- shift 8)
                            (cons (bitwise-and bits 255) res))
                          (next (+ 1 index) bits shift #f res))))
                    (next (+ 1 index) bits shift index-decode res))
                  (next (+ 1 index) bits shift value res)))
              (if (> value 0)
                (cons
                  (bitwise-and (bitwise-arithmetic-shift-left (bitwise-ior bits value) shift) 255)
                  res)
                res)))))))

  (define (base91-encode arg) "bytevector -> string"
    (list->string
      (reverse!
        (let (arg-length (bytevector-length arg))
          (let next ((index 0) (shift 0) (bits 0) (res (list)))
            (if (< index arg-length)
              (let
                (bits
                  (bitwise-ior bits
                    (bitwise-arithmetic-shift-left (bytevector-u8-ref arg index) shift)))
                (if (> (+ 8 shift) 13)
                  (let (value (bitwise-and bits 8191))
                    (if (> value 88)
                      (next (+ 1 index) (- shift 5)
                        (bitwise-arithmetic-shift-right bits 13)
                        (cons* (vector-ref chars-encode (truncate (/ value 91)))
                          (vector-ref chars-encode (modulo value 91)) res))
                      (next (+ 1 index) (- shift 6)
                        (bitwise-arithmetic-shift-right bits 14)
                        (let (value (bitwise-and bits 16383))
                          (cons* (vector-ref chars-encode (truncate (/ value 91)))
                            (vector-ref chars-encode (modulo value 91)) res)))))
                  (next (+ 1 index) (+ 8 shift) bits res)))
              (if (> shift 0)
                ( (l (res)
                    (if (or (> shift 7) (> bits 90))
                      (cons (vector-ref chars-encode (truncate (/ bits 91))) res) res))
                  (cons (vector-ref chars-encode (modulo bits 91)) res))
                res))))))))