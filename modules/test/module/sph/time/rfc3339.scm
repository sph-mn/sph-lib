(define-test-module (test module sph time rfc3339)
  (import
    (sph time rfc3339))

  (test-execute-procedures-lambda
    (time-rfc3339->seconds-and-deciseconds "2003-12-13T18:30:02.25555-01:00" (1071343802 . 25555)
      "2003-12-13T18:30:02.25Z" (1071340202 . 25))
    (time-rfc3339->seconds "2003-12-13T18:30:02+01:00" 1071336602
      "2003-12-13T18:30:02-01:00" 1071343802
      "2003-12-13T18:30:02.25Z" 1071340202 "2003-12-13T18:30:02Z" 1071340202)))
