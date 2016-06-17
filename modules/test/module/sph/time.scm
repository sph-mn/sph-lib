(define-test-module (test module sph time)
  (import
    (sph time))

  (define 2015-12-28 1451260835)
  ;2016 had 52 weeks and was a leap year
  (define 2016-1-1 1451606435)
  (define 2016-1-4 1451865635)
  (define 2016-6-17 1466121635)
  (define 2016-6-13 1465776035)
  (define 2016-6-1 1464739235)
  ;1981 had 53 weeks and was not a leap year
  (define 1981-12-31 378604820)
  (define 1981-12-1 376012820)
  (define 1981-1-1 347155219)
  (define 1981-12-28 378345620)
  ;1992 had 53 weeks and was a leap year
  (define 1992-1-1 694224026)
  (define 2016-6-17-11-32-59 1466163214)
  (define-test (time-local-utc-offset) (integer? (time-local-utc-offset)))

  (test-execute-procedures-lambda time-local-utc-offset
    (time-year-start (unquote 2016-6-17) (unquote 2016-1-1)
      (unquote 2016-1-1) (unquote 2016-1-1)
      (unquote 2016-1-4) (unquote 2016-1-1) (unquote 1981-12-31) (unquote 1981-1-1))
    (time-day-start (unquote (+ 101 2016-1-1)) (unquote 2016-1-1)
      (unquote (+ 201 2016-6-17)) (unquote 2016-6-17)
      (unquote (+ 301 2016-1-4)) (unquote 2016-1-4) (unquote (+ 401 1981-12-31)) (unquote 1981-12-31))
    (time-month-start (unquote 2016-1-1) (unquote 2016-1-1)
      (unquote 2016-6-17) (unquote 2016-6-1)
      (unquote 2016-1-4) (unquote 2016-1-1) (unquote 1981-12-31) (unquote 1981-12-1))
    (time-week-start (unquote 2016-6-17) (unquote 2016-6-13)
      (unquote 2016-1-1) (unquote 2015-12-28)
      (unquote 2016-1-4) (unquote 2016-1-4) (unquote 1981-12-31) (unquote 1981-12-28))
    (time->day (unquote 2016-6-17) 17
      (unquote 2016-1-1) 1 (unquote 2016-1-4) 4 (unquote 1981-12-31) 31)
    (time->month (unquote 2016-6-17) 6 (unquote 2016-1-1) 1 (unquote 2016-1-4) 1)
    (time->year (unquote 2016-6-17) 2016
      (unquote 2016-1-1) 2016 (unquote 2016-1-4) 2016 (unquote 1981-12-31) 1981)
    (time-year-weeks-53? (unquote 2016-6-17) #f
      (unquote 2016-1-4) #f (unquote 1981-12-31) #t (unquote 2015-12-28) #t)
    (time->week (unquote 2016-6-17) 24
      (unquote 2016-1-4) 1 (unquote 1981-12-31) 53 (unquote 2016-1-1) 53)
    (time->week-day (unquote 2016-6-17) 4
      (unquote 2016-1-4) 0 (unquote 1981-12-31) 3 (unquote 2016-1-1) 4)
    (time-leap-year? (unquote 2016-6-17) #t
      (unquote 2015-12-28) #f (unquote 2016-1-4) #t (unquote 1981-12-31) #f (unquote 2016-1-1) #t)
    (time-leap-year-number? 2016 #t 1981 #f 1982 #f 2015 #f)
    (time-day (unquote 2016-6-17-11-32-59) 41579) (time-month (unquote 2016-6-17-11-32-59) 1423979)
    (time-year (unquote 2016-6-17-11-32-59) 14556779)
    (time-from-ymdhms
      (#:year 2016 #:month
        6 #:day 17 #:hour 11 #:minute 32 #:second 59 #:offset-hour -1 #:offset-minute -1)
      (unquote (+ 2016-6-17-11-32-59 time-seconds-hour time-seconds-minute)))))
