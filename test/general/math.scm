(import (sph) (sph test) (sph math))

(define (test-signed-integer-binary-length inp exp)
  (assert-equal (max 2 (+ (number-length inp 2) (if (< inp 0) 0 1)))
    (signed-integer-binary-length inp)))

(execute-tests-quasiquote
  (signed-integer-binary-length 1 #t
    0 #t 8 #t 127 #t 255 #t 256 #t 1024 #t -1 #t -8 #t -127 #t -128 #t -1024 #t))