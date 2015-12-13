(define-test-module (test module sph base64)
  (import
    (rnrs bytevectors)
    (sph base64))

  (define-test (base64-encode+decode inp exp)
    (let (r (base64-decode (base64-encode (first inp))))
      (if (equal? (bytevector=? (first inp) r) exp) exp r)))

  (test-execute-procedures-lambda
    (base64-encode+decode
      #vu8(49 2 165 27 17 225 73 188 10 59 147 8 17 68 143 81 145 86 210 94 41 22 13 13) #t)))
