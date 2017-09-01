(define-test-module (test module sph time string)
  (import
    (sph time)
    (sph time utc)
    (sph time string))

  (define-test (time->iso8601-datetime) (string? (time->ymd)))
  (define 2016-6-17 (utc-from-date (date-create* 2016 6 17)))
  (define-test (utc-current-ymd-ks) (string? (utc-current-ymd-ks)))

  (test-execute-procedures-lambda (utc->ymd (unquote 2016-6-17) "2016-06-17")
    (utc-duration-from-hms (3 20 10) 12010 (0 20 10) 1210 (0 0 0) 0) utc-current-ymd-ks
    (utc-duration->hms -60 (0 -1 0)
      60 (0 1 0)
      3600 (1 0 0)
      3660 (1 1 0)
      3661 (1 1 1)
      -3600 (-1 0 0) -3660 (-1 -1 0) -3661 (-1 -1 -1) 12010 (3 20 10) 1210 (0 20 10) 0 (0 0 0))))
