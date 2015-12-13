(define-test-module (test module sph log)
  (import  (sph list) (guile) (sph log))
  (define output-string (open-output-string))
  (define output-string-route (vector (q (all b c)) log-default-formatter (list output-string)))
  (set! log-routes (list output-string-route))

  (define (test-log-message inp exp) (apply log-message inp)
    (let (r (get-output-string output-string)) (if (eqv? (> (string-length r) 0) exp) exp r)))

  (test-execute-procedures-lambda (log-message ((a b) "m1" "m2") #f ((c b) "m1" "m2") #t)))
