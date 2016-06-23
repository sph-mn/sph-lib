(library (sph time)
  (export
    time->date
    time->utc
    time-current
    time-date
    time-days
    time-from-utc
    time-nanoseconds->seconds
    time-seconds->nanoseconds
    time-utc-from-year
    time-year)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph one)
    (sph record)
    (sph time gregorian)
    (sph time utc))

  ;time as integers of tai nanoseconds since the unix epoch. selected conversions for the gregorian calendar, utc and iso8601
  (define (time-seconds->nanoseconds a) (* 1000000000 a))
  (define (time-nanoseconds->seconds a) (floor (/ a 1000000000)))
  (define-record time-date year month day hour minute second nanosecond)
  (define greg-years-1970-days 719527)

  (define (time-current)
    (let* ((a (gettimeofday)) (seconds (first a)) (microseconds (tail a)))
      (* 1000 (+ (* seconds 1000000) microseconds))))

  (define (time->utc a) (- a (time-seconds->nanoseconds (utc-tai->leap-second-difference a))))
  (define (time-from-utc a) (+ a (time-seconds->nanoseconds (utc-tai->leap-second-difference a))))
  (define (time-days a) (/ (time->utc a) utc-nanoseconds-day))

  (define (time-year a)
    (let (days (quotient (time->utc a) utc-nanoseconds-day))
      (+ 1970 (truncate (/ (- days (greg-days->leap-days days)) greg-year-days)))))

  (define (time-utc-from-year a)
    (* utc-nanoseconds-day (+ (* a greg-year-days) (greg-year->leap-days a))))

  (define (time->date a)
    (let (a-utc (time->utc a))
      (call-with-values (thunk (truncate/ a-utc utc-nanoseconds-day))
        (l (days day-rest)
          (let
            ( (days (+ greg-years-1970-days days))
              (hour (quotient day-rest (time-seconds->nanoseconds 3600))))
            (call-with-values (thunk (truncate/ (- days (greg-days->leap-days days)) 365))
              (l (year days-rest) (debug-log days-rest)
                (let
                  (days-per-month
                    (if (greg-year-leap-year? year) greg-month-days-leap-year greg-month-days))
                  (greg-year-days->month-and-day& days-rest days-per-month
                    (l (month month-day) (list year month month-day hour))))))))))))
