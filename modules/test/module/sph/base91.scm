(define-test-module (test module sph base91)
  (import (rnrs bytevectors) (sph base91))

  (define-test (base91 a)
    (let*
      ((in (string->utf8 (first a))) (encoded (base91-encode in)) (out (base91-decode encoded)))
      (or (equal? in out) (list (q in) in (q out) out (q encoded) encoded))))

  (test-execute-procedures-lambda
    (base91
      ; previous bug
      "tata" #t
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!#$%&()*+,./:;<=>?@[]^_`{|}~\"" #t
      "" #t
      )))
