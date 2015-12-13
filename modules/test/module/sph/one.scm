(define-test-module (test module sph one)
  (import
    (sph one)
    (sph number)
    (rnrs bytevectors))

  (define-test (integer->bytevector inp exp)
    (= (first inp)
      (bytevector-sint-ref (integer->bytevector (first inp)) 0
        (native-endianness) (bit->byte-length (number-container-length (abs (first inp)) 2)))))

  (test-execute-procedures-lambda
    (integer->bytevector 1383213160 #t -1383213160 #t)
    (in-between?
      (3 4 5) #f
      (4 3 5) #t
      (4 2 7) #t
      (0 0 1) #f
      (2 4 1) #f)))
