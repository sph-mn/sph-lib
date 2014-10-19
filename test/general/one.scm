(import
  (sph one)
  (sph test)
  (sph math)
  (rnrs bytevectors))

(define (test-integer->bytevector inp exp)
  (= inp
    (bytevector-sint-ref (integer->bytevector inp) 0
      (native-endianness) (bit->byte-length (signed-integer-binary-length inp)))))

(execute-tests-quasiquote
  (integer->bytevector 1383213160 #t -1383213160 #t)
  (in-between?
    (3 4 5) #f
    (4 3 5) #t
    (4 2 7) #t
    (0 0 1) #f
    (2 4 1) #f))
