(define-test-module (test module sph time string)
  (import
    (sph time)
    (sph time string))

  ;(define-test (current-local-datetime-string) (string? (current-local-datetime-string)))

  (l (settings) #t)
  #;(test-execute-procedures-lambda


    current-local-datetime-string
    (hms->timu (3 20 10) 12010 (0 20 10) 1210 (0 0 0) 0)
    (time->hms -60 (0 -1 0)
      60 (0 1 0)
      3600 (1 0 0)
      3660 (1 1 0)
      3661 (1 1 1)
      -3600 (-1 0 0) -3660 (-1 -1 0) -3661 (-1 -1 -1) 12010 (3 20 10) 1210 (0 20 10) 0 (0 0 0))))
