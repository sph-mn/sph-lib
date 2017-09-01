(define-test-module (test module sph time)
  (import
    (sph time)
    (sph record)
    (sph time gregorian)
    (sph time utc))

  ;-- utc times
  (define 2015-12-28 (seconds->nanoseconds 1451260800))
  ; 2016 had 52 weeks and was a leap year
  (define 2016-1-1 (seconds->nanoseconds 1451606400))
  (define 2016-1-3 (s->ns 1451779236))
  (define 2016-1-4 (seconds->nanoseconds 1451865600))
  (define 2016-6-17 (seconds->nanoseconds 1466121600))
  (define 2016-6-13 (seconds->nanoseconds 1465776000))
  (define 2016-6-1 (seconds->nanoseconds 1464739200))
  ; 1981 had 53 weeks and was not a leap year
  (define 1981-12-1 376012800000000000)
  (define 1981-1-1 (seconds->nanoseconds 347155200))
  (define 1981-12-27 (seconds->nanoseconds 378259200))
  (define 1981-12-28 (seconds->nanoseconds 378345600))
  (define 1981-12-31 (seconds->nanoseconds 378604800))
  ; a leap second occured between the following dates
  (define 1972-12-31 94608000000000000)
  (define 1973-1-1 94694400000000000)
  ; 1992 had 53 weeks and was a leap year
  (define 1992-1-1 (seconds->nanoseconds 694224000))
  (define 2016-6-17-11-32-59 (seconds->nanoseconds 1466163179))
  (define 2000-12-31 978220800000000000)
  (define 1981-1-5 347500819000000000)
  (define 1-12-31 (s->ns -62104147200))
  ; the date for -1-1-1 could not be confirmed with the "date" system utility or "srfi-19" (both use year 0 btw),
  ; but the the calculation using from-date seems correct.
  (define negative-0-1-1 -62167219200000000000)
  (define negative-1-5-10 -62187609600000000000)
  (define negative-2-4-10 -62190115200000000000)
  (define negative-7-12-28 -62356867200000000000)
  (define negative-2001-1-1 -125312659200000000000)
  (define-test (local-utc-offset) (integer? (utc-zone-offset)))

  (define-test (greg-days->years-2)
    (let loop ((year 1))
      (if (<= year 1200)
        (let (year-calculated (+ 1 (greg-days->years (greg-years->days (- year 1)))))
          (if (= year year-calculated) (loop (+ 1 year)) (assert-equal year year-calculated)))
        #t)))

  (define-test (greg-days->leap-days-2)
    "this test combines greg-days->leap-days with greg-years->leap-days and greg-years->days"
    (let loop ((years 0))
      (if (< years 3000)
        (let
          ( (expected (greg-years->leap-days years))
            (result (greg-days->leap-days (greg-years->days years))))
          (if (equal? expected result) (loop (+ 1 years))
            (list (q result) result (q expected) expected (q years) years)))
        #t)))

  (define-test (greg-days->leap-days-3)
    (let loop ((days 0))
      (if (< days 999999)
        (let
          ( (expected (greg-years->leap-days (+ 1 (greg-days->years days))))
            (result (greg-days->leap-days days)))
          (if (or (equal? expected result) (= 1 (- expected result))) (loop (+ 1 days))
            (list (q expected) expected
              (q result) result (q year) (+ 1 (greg-days->years days)) (q days) days)))
        #t)))

  (define-test (greg-year-days->month-and-day& arguments)
    (let (days (first arguments))
      (greg-year-days->month-and-day& days (greg-month-days-get #f) list)))

  (define-test (utc->date-2)
    "creates a time record for each day, converts it time and then back to a day and compares
    the result with the input"
    (let loop ((year -2002) (month 1) (day 1))
      (if (< year 2002)
        (let*
          ( (expected (record date-record year month day 0 0 0 0 0))
            (leap-year? (greg-year-leap-year? year)) (days-in-month (greg-month-days-get leap-year?))
            (day-count (vector-ref days-in-month (- month 1)))
            (date (utc->date (utc-from-date expected))))
          (if (equal? expected date)
            (loop (if (and (= month 12) (= day day-count)) (+ 1 year) year)
              (if (= day day-count) (+ 1 (modulo month 12)) month) (+ 1 (modulo day day-count)))
            (list (q expected) expected (q result) date (q time) (utc-from-date expected))))
        #t)))

  (define-test (greg-week-day)
    (let (end 9999)
      (let loop ((year 1))
        (if (<= year end)
          (if (= (greg-year-first-week-day year) (greg-week-day year 1 1)) (loop (+ 1 year)) #f) #t))))

  (define-test (utc-start-first-week arguments)
    (utc->date (utc-start-first-week (first arguments))))

  (define-test (utc-start-last-week arguments) (utc->date (utc-start-last-week (first arguments))))

  (test-execute-procedures-lambda (greg-years->year 0 1 1 2 3 4 -1 0 -2 -1)
    (greg-year->years 1 0 2 1 0 -1 -1 -2 -2 -3 -7 -8 -99 -100)
    (greg-years->leap-days 0 0
      1 0
      3 0
      4 1
      5 1
      7 1
      8 2
      9 2
      101 24
      104 25
      900 218
      96 24
      99 24
      100 24
      101 24
      102 24
      103 24
      104 25
      400 97
      800 194
      1200 291
      -97 25
      -5 2
      -1 1
      -2 1 -4 1 -8 2 -9 3 -88 22 -89 23 -96 24 -99 25 -98 25 -100 25 -101 25 -104 25 -105 26 -106 26)
    (greg-days->leap-days
      ;4 years without leap day (note: leap day is not at the end of the year)
      1460 1
      ;4 years with leap day
      1461 1
      1462 1
      ;near 100 years
      36159 24
      1826 1
      0 0
      1 0
      4 0
      365 0
      146097 97
      292194 194
      720256 478
      720257 478
      720258 478
      -1 0
      -366 1
      ;three years near february 29
      1153 0 1154 1
      ;four years near february 29
      -1766 1 -1768 2)
    (greg-days->year 365 2 364 1 1461 5 -1 0 -366 0 -365 0 -364 0 -367 -1 -731 -1 -732 -2)
    (greg-years->days 0 0
      1 365 4 1461 400 146097 800 292194 1970 719527 1980 723180 -1 -366 -4 -1461 -7 -2557)
    (greg-days->years 1460 3
      1461 4
      1 0
      365 1
      366 1 1826 5 146097 400 146098 400 723180 1980 723544 1980 723545 1981 -1 0 -366 -1 -2557 -7)
    (greg-year-leap-year? 2000 #t
      400 #t 300 #f 1972 #t 1992 #t 2016 #t 1981 #f 1970 #f 0 #t -4 #t -2000 #t)
    (utc-start-last-week (unquote 2016-6-17) #(2016 12 26 0 0 0 0 0)
      (unquote 1981-12-27) #(1981 12 28 0 0 0 0 0)
      (unquote 1981-1-1) #(1981 12 28 0 0 0 0 0) (unquote 1972-12-31) #(1972 12 25 0 0 0 0 0))
    (utc-start-year (unquote 2016-6-17) (unquote 2016-1-1)
      (unquote 2016-1-1) (unquote 2016-1-1)
      (unquote 2016-1-4) (unquote 2016-1-1) (unquote 1981-12-31) (unquote 1981-1-1))
    (utc-start-day (unquote (+ 101 2016-1-1)) (unquote 2016-1-1)
      (unquote (+ 201 2016-6-17)) (unquote 2016-6-17)
      (unquote (+ 301 2016-1-4)) (unquote 2016-1-4) (unquote (+ 401 1981-12-31)) (unquote 1981-12-31))
    (utc-start-month (unquote 2016-1-1) (unquote 2016-1-1)
      (unquote 2016-6-17) (unquote 2016-6-1)
      (unquote 2016-1-4) (unquote 2016-1-1) (unquote 1981-12-31) (unquote 1981-12-1))
    (utc-start-week (unquote 2016-6-17) (unquote 2016-6-13)
      (unquote 2016-1-1) (unquote 2015-12-28)
      (unquote 2016-1-4) (unquote 2016-1-4) (unquote 1981-12-31) (unquote 1981-12-28))
    (greg-year-weeks-53? 2016 #f 1981 #t 2015 #t) (greg-week-day)
    (greg-month->days (1 #f) 0 (2 #f) 31 (3 #f) 59 (3 #t) 60 (12 #f) 334) (greg-days->years-2)
    (greg-year-days->month-and-day& 0 (1 1) 1 (1 2) 3 (1 4) 31 (2 1) 364 (12 31))
    (utc-start-first-week (unquote 2016-6-17) #(2016 1 4 0 0 0 0 0)
      (unquote 1981-12-27) #(1980 12 29 0 0 0 0 0)
      (unquote 1981-1-1) #(1980 12 29 0 0 0 0 0) (unquote 1972-12-31) #(1972 1 3 0 0 0 0 0))
    (utc->years (unquote 2016-1-1) 2015
      (unquote 2016-1-3) 2015
      (unquote negative-0-1-1) -1 (unquote negative-1-5-10) -1 (unquote negative-7-12-28) -7)
    (utc->week (unquote 1-12-31) 1
      (unquote 1981-1-5) 2
      (unquote 1981-1-1) 1
      (unquote 2016-1-1) 53
      (unquote 2016-1-3) 53
      (unquote 2016-6-17) 24
      (unquote 2016-1-4) 1
      (unquote 1981-12-31) 53
      (unquote 1981-12-28) 53
      (unquote 1981-12-27) 52 (unquote negative-1-5-10) 19 (unquote negative-0-1-1) 52)
    (utc->week-day (unquote 2016-6-17) 4
      (unquote 2016-1-4) 0 (unquote 1981-12-31) 3 (unquote 2016-1-1) 4)
    (utc-from-date #(1970 1 1 0 0 0 0 0) 0
      #(2016 1 1 0 0 0 0 0) (unquote 2016-1-1)
      #(1981 1 1 0 0 0 0 0) (unquote 1981-1-1)
      #(1981 12 1 0 0 0 0 0) (unquote 1981-12-1)
      #(1973 1 1 0 0 0 0 0 0) (unquote 1973-1-1)
      #(1972 12 31 0 0 0 0 0 0) (unquote 1972-12-31)
      #(2015 12 28 0 0 0 0 0 0) (unquote 2015-12-28)
      #(2016 6 17 0 0 0 0 0) (unquote 2016-6-17)
      #(1992 1 1 0 0 0 0 0) (unquote 1992-1-1)
      #(0 1 1 0 0 0 0 0) (unquote negative-0-1-1)
      #(-1 5 10 0 0 0 0 0) (unquote negative-1-5-10) #(-7 12 28 0 0 0 0 0) (unquote negative-7-12-28))
    (utc->date (unquote 2000-12-31) #(2000 12 31 0 0 0 0 0)
      (unquote 1981-12-1) #(1981 12 1 0 0 0 0 0)
      (unquote 2016-6-17-11-32-59) #(2016 6 17 11 32 59 0 0)
      (unquote 2015-12-28) #(2015 12 28 0 0 0 0 0)
      (unquote 2016-1-1) #(2016 1 1 0 0 0 0 0)
      (unquote 1972-12-31) #(1972 12 31 0 0 0 0 0)
      (unquote 1973-1-1) #(1973 1 1 0 0 0 0 0) (unquote negative-2001-1-1) #(-2001 1 1 0 0 0 0 0))
    ;(greg-days->leap-days-2) (greg-days->leap-days-3)
    ;(utc->date-2)
    ))
