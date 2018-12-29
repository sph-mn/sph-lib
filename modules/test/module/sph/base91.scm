(define-test-module (test module sph base91)
  (import (rnrs bytevectors) (sph base91))
  (define-test (base91-decode arguments) (utf8->string (apply base91-decode arguments)))
  (test-execute-procedures-lambda
    (base91-encode (unquote (string->utf8 "test")) "fPNKd")
    (base91-decode "fPNKd" "test")))
