(import
  (rnrs bytevectors)
  (sph)
  (sph test-old)
  (sph random-data)
  (sph base91))

(define (test-base91-decode inp exp)
  (utf8->string (base91-decode inp)))

(execute-tests-quasiquote
  (base91-encode
    (unquote (string->utf8 "test"))
    "fPNKd")
  (base91-decode "fPNKd" "test"))