(import
  (sph)
  (sph test-old)
  (sph time))

(define (test-current-local-datetime-string)
  (string? (current-local-datetime-string)))

(execute-tests-quasiquote
  (current-local-datetime-string))