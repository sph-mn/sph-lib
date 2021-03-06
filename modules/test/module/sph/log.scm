(define-test-module (test module sph log)
  (import (sph list) (sph log))

  (define output-string (open-output-string))
  (define output-string-route (vector (q (and b c)) log-default-formatter (list output-string)))
  (log-routes-set! (list output-string-route))

  (define-test (log-message arguments expected) (apply log-message arguments)
    (let (r (get-output-string output-string))
      (if (eqv? (> (string-length r) 0) expected) expected r)))

  (test-execute-procedures-lambda (log-message ((a b) "m1" "m2") #f ((c b) "m1" "m2") #t)))
