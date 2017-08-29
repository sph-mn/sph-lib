(define-test-module (test module sph one)
  (import
    (sph one)
    (sph number)
    (rnrs bytevectors))

  (define-test (integer->bytevector inp exp)
    (= (first inp)
      (bytevector-sint-ref (integer->bytevector (first inp)) 0
        (native-endianness) (bit->byte-length (number-container-length (abs (first inp)) 2)))))

  (test-execute-procedures-lambda (integer->bytevector 1383213160 #t -1383213160 #t)))
