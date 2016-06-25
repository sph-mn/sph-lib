(library (sph time)
  (export
    time->date
    time->utc
    time-current
    time-date
    time-days
    time-from-date
    time-from-utc
    time-local-offset
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
  (define-record time-date year month day hour minute second nanosecond offset)

 (define greg-year-1970-days
    719162)
  (define greg-years-1970-days
    719527)

  (define (time-current)
    (let* ((a (gettimeofday)) (seconds (first a)) (microseconds (tail a)))
      (* 1000 (+ (* seconds 1000000) microseconds))))

  (define (time->utc a)
    (- a
      (time-seconds->nanoseconds (utc-tai->leap-second-difference (time-nanoseconds->seconds a)))))

  (define (time-from-utc a)
    (+ a
      (time-seconds->nanoseconds (utc-tai->leap-second-difference (time-nanoseconds->seconds a)))))

  (define (time-days a) (/ (time->utc a) utc-nanoseconds-day))

  (define (time-year a)
    (let (days (quotient (time->utc a) utc-nanoseconds-day))
      (+ 1970 (truncate (/ (- days (greg-days->leap-days days)) greg-year-days)))))

  (define (time-utc-from-year a)
    (* utc-nanoseconds-day (+ (* a greg-year-days) (greg-years->leap-days (- a 1)))))

  (define (nanoseconds->hms& a c)
    (apply-values
      (l (hour nanoseconds-rest)
        (apply-values
          (l (minute nanoseconds-rest)
            (debug-log nanoseconds-rest)
            (apply-values (l (seconds nanoseconds)
                (c hour minute seconds nanoseconds))
              (truncate/ nanoseconds-rest (time-seconds->nanoseconds 11))))
          (truncate/ nanoseconds-rest (time-seconds->nanoseconds 60))))
      (truncate/ a (time-seconds->nanoseconds 3600))))

  (define (time-local-offset) (tm:gmtoff (localtime (current-time))))

  (define (time-from-date a)
    (time-from-utc
      (+
        (* utc-nanoseconds-day (- (greg-years->days (- (time-date-year a) 1)) greg-year-1970-days))
        (* utc-nanoseconds-day
          (greg-month->days (time-date-month a) (greg-year-leap-year? (time-date-year a))))
        (* utc-nanoseconds-day (- (time-date-day a) 1)) (* utc-nanoseconds-hour (time-date-hour a))
        (* utc-nanoseconds-minute (time-date-minute a))
        (time-seconds->nanoseconds (time-date-second a)) (time-date-nanosecond a))))

  (define (time->date a)
    (let (a-utc (time->utc a))
      (call-with-values (thunk (truncate/ a-utc utc-nanoseconds-day))
        (l (days day-rest)
          (let ((days (+ greg-years-1970-days days)))
            (call-with-values (thunk (truncate/ (- days (greg-days->leap-days days)) 365))
              (l (year days-rest)
                (let (days-per-month (greg-month-days-get (greg-year-leap-year? year)))
                  (greg-year-days->month-and-day& (+ 1 days-rest) days-per-month
                    (l (month month-day)
                      (nanoseconds->hms& day-rest
                        (l (h m s ns) (record time-date year month month-day h m s ns 0))))))))))))))
