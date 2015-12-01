(import
  (sph)
  (sph test)
  (sph time))

(define (test-current-local-datetime-string)
  (string? (current-local-datetime-string)))

(execute-tests-quasiquote
  (current-local-datetime-string))