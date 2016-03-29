(define-test-module (test module sph time)
  (import
    (sph time))

  (define-test (current-local-datetime-string)
    (string? (current-local-datetime-string)))

  (test-execute-procedures-lambda
    current-local-datetime-string

    (rfc3339-date->seconds
      "2003-12-13T18:30:02+01:00" 0
      ;"2003-12-13T18:30:02.25Z" 0
      ;"2003-12-13T18:30:02.25+01:00" 0
      ;"2003-12-13T18:30:02Z" 0

      )


    ))
