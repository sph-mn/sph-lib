(define-test-module (test module sph time)
  (import
    (sph time)
    (sph time gregorian)
    (sph time utc))

  (define 2015-12-28 (time-seconds->nanoseconds 1451260835))
  ;2016 had 52 weeks and was a leap year
  (define 2016-1-1 (time-seconds->nanoseconds 1451606435))
  (define 2016-1-4 (time-seconds->nanoseconds 1451865635))
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

  (define-test (greg-days->year)
    (let loop ((year 1))
      (if (<= year 1200)
        (if (= year (greg-days->year (greg-year->days year))) (loop (+ 1 year))
          (assert-equal year (greg-days->year (greg-year->days year))))
        #t)))

  (define-test (greg-days->leap-days-2 arguments)
    (greg-days->leap-days (greg-year->days (first arguments))))

  (test-execute-procedures-lambda (greg-year->days 400 146097 800 292194 1 365 5 1826 0 0)
    (greg-year->leap-days 400 97 800 194 1200 291 0 0 1 0 2 0 3 0 4 1 5 1 8 2 9 2)
    (greg-days->leap-days 0 0 1 0 4 0 365 0 1826 1 146097 97 292194 194)
    (greg-days->leap-days-2 0 0 1 0 2 0 3 0 4 1 5 1 8 2 9 2 400 97 800 194 1200 291)
    (greg-month->ordinal-day (1 #f) 1 (3 #f) 60 (3 #t) 61 (12 #f) 335) greg-days->year
    (time-from-date #(2015 12 28 0 0 0 0 0 0) (unquote 2015-12-28))
    (time->date (unquote 2015-12-28) #(2015 12 28 0 0 0 0 0)))

  #;(test-execute-procedures-lambda time-local-utc-offset
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
    (time->year
      (unquote (time-from-year 1979)) 1979
      (unquote 2016-6-17) 2016
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
