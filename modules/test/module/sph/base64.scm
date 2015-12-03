(import
  (sph)
  (rnrs bytevectors)
  (sph test-old)
  (sph base64))

(define (test-base64-encode+decode inp exp)
  (let (r (base64-decode (base64-encode inp)))
    (if (equal? (bytevector=? inp r) exp) exp r)))

(execute-tests-quasiquote
  (base64-encode+decode
    #vu8(49 2 165 27 17 225 73 188 10 59 147 8 17 68 143 81 145 86 210 94 41 22 13 13) #t))
