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
  (define 1992-1-1 )

  (test-execute-procedures-lambda
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
    (time-year-weeks-53? (unquote 2016-6-17) #f (unquote 2016-1-4) #f (unquote 1981-12-31) #t (unquote 2015-12-28) #t)
    (time->week
      (unquote 2016-6-17) 24
      (unquote 2016-1-4) 1 (unquote 1981-12-31) 53
      (unquote 2016-1-1) 53 )))
