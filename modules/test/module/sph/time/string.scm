(define-test-module (test module sph time string)
  (import
    (sph time)
    (sph time utc)
    (sph time string))

  (define-test (time->iso8601-datetime) (string? (time->iso8601-ymd)))
  (define 2016-6-17 (time-from-date (time-make-date #:year 2016 #:month 6 #:day 17)))
  (define-test (time-current-iso8601-ks-string) (string? (time-current-iso8601-ks-string)))

  (test-execute-procedures-lambda (time->iso8601-ymd (unquote 2016-6-17) "2016-06-17")
    (utc-duration-from-hms (3 20 10) 12010 (0 20 10) 1210 (0 0 0) 0) time-current-iso8601-ks-string
    (utc-duration->hms -60 (0 -1 0)
      60 (0 1 0)
      3600 (1 0 0)
      3660 (1 1 0)
      3661 (1 1 1)
      -3600 (-1 0 0) -3660 (-1 -1 0) -3661 (-1 -1 -1) 12010 (3 20 10) 1210 (0 20 10) 0 (0 0 0))))
