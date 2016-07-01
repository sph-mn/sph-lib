(library (sph time)
  (export
    time->date
    time->days
    time->hours
    time->minutes
    time->seconds
    time->utc
    time->week
    time->week-day
    time->years
    time-current
    time-date
    time-date->week-day
    time-date-day
    time-date-hour
    time-date-increment-day
    time-date-minute
    time-date-month
    time-date-nanosecond
    time-date-offset
    time-date-second
    time-date-week-count
    time-date-year
    time-days
    time-elapsed-day
    time-elapsed-hour
    time-elapsed-minute
    time-elapsed-month
    time-elapsed-year
    time-from-date
    time-from-days
    time-from-hours
    time-from-minutes
    time-from-utc
    time-from-years
    time-local-offset
    time-make-date
    time-make-date*
    time-nanoseconds->seconds
    time-ns->s
    time-s->ns
    time-seconds->nanoseconds
    time-start-day
    time-start-first-week
    time-start-hour
    time-start-last-week
    time-start-minute
    time-start-month
    time-start-second
    time-start-week
    time-start-year
    time-utc-from-year
    time-week-first
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
  (define time-ns->s time-nanoseconds->seconds)
  (define time-s->ns time-seconds->nanoseconds)
  (define-record time-date year month day hour minute second nanosecond offset)
  (define greg-year-1970-days 719162)
  (define greg-years-1970-days 719527)

  (define*
    (time-make-date #:key (year 1) (month 1) (day 1) (hour 0) (minute 0) (second 0) (nanosecond 0)
      (offset 0))
    (record time-date year month day hour minute second nanosecond offset))

  (define*
    (time-make-date* #:optional (year 1) (month 1) (day 1) (hour 0) (minute 0) (second 0)
      (nanosecond 0)
      (offset 0))
    (record time-date year month day hour minute second nanosecond offset))

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
    (* utc-nanoseconds-day (+ (* a greg-year-days) (greg-years->leap-days (greg-year->years a)))))

  (define (nanoseconds->hms& a c)
    (apply-values
      (l (hour nanoseconds-rest)
        (apply-values
          (l (minute nanoseconds-rest)
            (apply-values (l (seconds nanoseconds) (c hour minute seconds nanoseconds))
              (truncate/ nanoseconds-rest (time-seconds->nanoseconds 1))))
          (truncate/ nanoseconds-rest (time-seconds->nanoseconds 60))))
      (truncate/ a (time-seconds->nanoseconds 3600))))

  (define (time-local-offset) (tm:gmtoff (localtime (current-time))))

  (define (time-from-date a)
    (time-from-utc
      (+
        (* utc-nanoseconds-day
          (- (greg-years->days (greg-year->years (time-date-year a))) greg-year-1970-days))
        (* utc-nanoseconds-day
          (greg-month->days (time-date-month a) (greg-year-leap-year? (time-date-year a))))
        (* utc-nanoseconds-day (- (time-date-day a) 1)) (* utc-nanoseconds-hour (time-date-hour a))
        (* utc-nanoseconds-minute (time-date-minute a))
        (time-seconds->nanoseconds (time-date-second a)) (time-date-nanosecond a)
        (* (time-seconds->nanoseconds (time-date-offset a)) -1))))

  (define (time-days-and-rest& a c)
    (apply-values (l (days day-rest) (c (+ greg-year-1970-days days) day-rest))
      (truncate/ (time->utc a) utc-nanoseconds-day)))

  (define (time->date a)
    (time-days-and-rest& a
      (l (days day-rest)
        (let*
          ( (years (greg-days->years days)) (year (greg-days->year days))
            (leap-year? (greg-year-leap-year? year))
            (days (greg-days->year-days (- days (greg-years->days years)) leap-year?))
            (days-per-month (greg-month-days-get leap-year?)))
          (greg-year-days->month-and-day& days days-per-month
            (l (month month-day)
              (nanoseconds->hms& day-rest
                (l (h m s ns) (record time-date year month month-day h m s ns 0)))))))))

  (define (time-from-years a)
    (time-from-utc (* utc-nanoseconds-day (- (greg-years->days a) greg-year-1970-days))))

  (define (time-from-days a) (time-from-utc (* utc-nanoseconds-day (- a greg-year-1970-days))))
  (define (time-from-hours a) (* utc-nanoseconds-hour a))
  (define (time-from-minutes a) (* utc-nanoseconds-minute a))
  (define (time->days a) (/ (time->utc a) utc-nanoseconds-day))

  (define (time->years a)
    (greg-days->years (+ greg-year-1970-days (time->days a))))

  (define (time->hours a) (/ (time->utc a) utc-nanoseconds-hour))
  (define (time->minutes a) (/ (time->utc a) utc-nanoseconds-minute))
  (define (time->seconds a) (/ (time->utc a) 1000000000))

  (define (time-start-year a)
    (let (a (time->date a)) (time-from-date (time-make-date* (time-date-year a) 1 1))))

  (define (time-start-month a)
    (let (a (time->date a))
      (time-from-date (time-make-date* (time-date-year a) (time-date-month a) 1))))

  (define (time-start-day a)
    (let (a (time->date a))
      (time-from-date (time-make-date* (time-date-year a) (time-date-month a) (time-date-day a)))))

  (define (time-start-hour a)
    (let (a (time->date a))
      (time-from-date
        (time-make-date* (time-date-year a) (time-date-month a)
          (time-date-day a) (time-date-hour a)))))

  (define (time-start-minute a)
    (let (a (time->date a))
      (time-from-date
        (time-make-date* (time-date-year a) (time-date-month a)
          (time-date-day a) (time-date-hour a) (time-date-minute a)))))

  (define (time-start-second a)
    (let (a (time->date a))
      (time-from-date
        (time-make-date* (time-date-year a) (time-date-month a)
          (time-date-day a) (time-date-hour a) (time-date-minute a) (time-date-second a)))))

  (define (time-start-week a)
    (time-from-utc (- (time->utc a) (* (time->week-day a) utc-nanoseconds-day))))

  (define (time->week-day a) "from 0-6, with monday being the first day of the week"
    (let (a (time->date a))
      (greg-week-day (time-date-year a) (time-date-month a) (time-date-day a))))

  (define (time-date->week-day a) "from 0-6, with monday being the first day of the week"
    (greg-week-day (time-date-year a) (time-date-month a) (time-date-day a)))

  (define (time-elapsed-day a) (- a (time-start-day a)))
  (define (time-elapsed-year a) (- a (time-start-year a)))
  (define (time-elapsed-month a) (- a (time-start-month a)))
  (define (time-elapsed-hour a) (- a (time-start-hour a)))
  (define (time-elapsed-minute a) (- a (time-start-minute a)))

  (define (time-start-first-week a)
    "iso standard first week of current year of time.
    based on if thursday falls into the first week-days of the year"
    (time-from-utc
      (let* ((year-start (time-start-year a)) (week-day (time->week-day year-start)))
        (if (< week-day 4) (- (time->utc year-start) (* utc-nanoseconds-day week-day))
          (+ (time->utc year-start) (* utc-nanoseconds-day (- 7 week-day)))))))

  (define (time-start-last-week a)
    (time-from-utc
      (- (time->utc (time-start-first-week (time-add-years a 1))) utc-nanoseconds-week)))

  (define (time-date-week-count a) (if (greg-year-weeks-53? (time-date-year a)) 53 52))

  (define (time->week a) "integer -> integer"
    (let*
      ( (years (time->years a)) (year (greg-years->year years))
        (first-week (time-start-first-week a)) (difference (- a first-week)))
      ;(debug-log years (time->date first-week) difference)
      (if (= 0 difference) 1
        (if (< difference 0) (if (greg-year-weeks-53? (- year 1)) 53 52)
          (let (last-week (time-start-last-week a))
            (if (= a last-week) (if (greg-year-weeks-53? year) 53 52)
              (if (> a last-week)
                (if (>= (/ (- a last-week) utc-nanoseconds-week) 1) 1
                  (if (greg-year-weeks-53? year) 53 52))
                (let (weeks (/ difference utc-nanoseconds-week))
                  (if (integer? weeks) (+ 1 weeks) (ceiling weeks))))))))))

  (define (time-add-years a years)
    (time-from-utc
      (* utc-nanoseconds-day (- (greg-years->days (+ years (time->years a))) greg-year-1970-days))))

  (define (time-add-day a days) (time-from-utc (+ (* utc-nanoseconds-day days) (time->utc a))))
  (define (time-add-hours a hours) (+ (* utc-nanoseconds-hour hours) a))
  (define (time-add-minute a minutes) (+ (* utc-nanoseconds-minute minutes) a))
  (define (time-add-seconds a seconds) (+ (time-seconds->nanoseconds seconds) a))

  (define (time-add-weeks a weeks)
    (time-from-utc (+ (* utc-nanoseconds-day weeks 7) (time->utc a))))

  (define (time-date-increment-day a)
    (let (year (time-date-year a))
      (let*
        ( (month (time-date-month a)) (day (time-date-day a))
          (day-count (vector-ref (greg-month-days-get (greg-year-leap-year? year)) (- month 1))))
        (record time-date (if (and (= month 12) (= day day-count)) (+ 1 year) year)
          (if (= day day-count) (+ 1 (modulo month 12)) month) (+ 1 (modulo day day-count))
          (time-date-hour a) (time-date-minute a)
          (time-date-second a) (time-date-nanosecond a) (time-date-offset a))))))
