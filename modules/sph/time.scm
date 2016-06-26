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
    time-date-week-count
    time-days
    time-from-date
    time-from-utc
    time-local-offset
    time-make-date
    time-nanoseconds->seconds
    time-seconds->nanoseconds
    time-start-day
    time-start-hour
    time-start-minute
    time-start-month
    time-start-week
    time-start-year
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
  (define greg-year-1970-days 719162)
  (define greg-years-1970-days 719527)

  (define*
    (time-make-date #:optional (year 1) (month 1) (day 1) (hour 0) (minute 0) (second 0)
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
    (* utc-nanoseconds-day (+ (* a greg-year-days) (greg-years->leap-days (- a 1)))))

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
        (* utc-nanoseconds-day (- (greg-years->days (- (time-date-year a) 1)) greg-year-1970-days))
        (* utc-nanoseconds-day
          (greg-month->days (time-date-month a) (greg-year-leap-year? (time-date-year a))))
        (* utc-nanoseconds-day (- (time-date-day a) 1)) (* utc-nanoseconds-hour (time-date-hour a))
        (* utc-nanoseconds-minute (time-date-minute a))
        (time-seconds->nanoseconds (time-date-second a)) (time-date-nanosecond a))))

  (define (time-days-and-rest& a c)
    (apply-values (l (days day-rest) (c (+ greg-year-1970-days days) day-rest))
      (truncate/ (time->utc a) utc-nanoseconds-day)))

  (define (time->date a)
    (time-days-and-rest& a
      (l (days day-rest)
        (let*
          ( (years (greg-days->years days)) (year (+ years 1))
            (days (- days (greg-years->days years)))
            (days-per-month (greg-month-days-get (greg-year-leap-year? year))))
          (greg-year-days->month-and-day& days days-per-month
            (l (month month-day)
              (nanoseconds->hms& day-rest
                (l (h m s ns) (record time-date year month month-day h m s ns 0)))))))))

  (define (time->days a) (/ (time->utc a) utc-nanoseconds-day))
  (define (time->years a) (greg-days->years (time->days a)))
  (define (time->hours a) (/ (time->utc a) utc-nanoseconds-hour))
  (define (time->minutes a) (/ (time->utc a) utc-nanoseconds-minute))
  (define (time->seconds a) (/ (time->utc a) 1000000000))

  (define (time-start-year a)
    (* utc-nanoseconds-day (- (greg-years->days (truncate (time->years a))) greg-year-1970-days)))

  (define (time-start-month a)
    (let (a (time->date a))
      (time-from-date (time-make-date (time-date-year a) (time-date-month a) 1))))

  (define (time-start-day a)
    (let (a (time->date a))
      (time-from-date (time-make-date (time-date-year a) (time-date-month a) (time-date-day a)))))

  (define (time-start-hour a)
    (let (a (time->date a))
      (time-from-date
        (time-make-date (time-date-year a) (time-date-month a) (time-date-day a) (time-date-hour a)))))

  (define (time-start-minute a)
    (let (a (time->date a))
      (time-from-date
        (time-make-date (time-date-year a) (time-date-month a)
          (time-date-day a) (time-date-hour a) (time-date-minute a)))))

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

  (define (time-week-first a)
    "iso standard first week of current year of time.
    based on if thursday falls into the first week-days of the year"
    (let ((year-start (time-start-year a)) (week-day (time->week-day a)))
      (if (< week-day 3) (- year-start (+ 1 (* utc-nanoseconds-day week-day)))
        (+ year-start (* utc-nanoseconds-day (- 6 week-day))))))

  (define (time-add-years a years) (time-from-utc (greg-years->days (+ 1 (time->years a)))))

  (define (time-start-week a)
    (time-from-utc (- (time->utc a) (* (time->week-day a) utc-nanoseconds-day))))

  (define (time-date-week-count a) (if (greg-year-weeks-53? (time-date-year a)) 53 52))

  (define (time->week a) "integer -> integer"
    (let (difference (- a (time-week-first a)))
      (if (= 0 difference) 1
        (if (< difference 0) (if (time-year-weeks-53? (time-from-year (- (time->year a) 1))) 53 52)
          (let (weeks (/ difference time-seconds-week))
            ;any full week difference means the week has passed
            (if (= 0 (modulo difference time-seconds-week)) (+ 1 weeks) (ceiling weeks)))))))

  ;time-week-count
  ;time-add-year
  ;time-add-week
  ;time-add-month
  ;time-add-day
  ;time-add-hour
  ;time-add-minute
  ;time-subtract-year
  ;time-subtract-week
  ;time-subtract-month
  ;time-subtract-day
  ;time-subtract-hour
  ;time-subtract-minute
)
