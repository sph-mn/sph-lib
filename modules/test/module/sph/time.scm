(define-test-module (test module sph time)
  (import
    (sph time))

  (define-test (current-local-datetime-string)
    (string? (current-local-datetime-string)))

  (test-execute-procedures-lambda
    current-local-datetime-string))
