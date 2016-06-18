(library (sph time)
  (export
    time->date
    time->day
    time->hms
    time->month
    time->week
    time->week-day
    time->year
    time-current
    time-day
    time-day-start
    time-days->seconds
    time-from-date
    time-from-hms
    time-from-year
    time-from-ymdhms
    time-leap-year-number?
    time-leap-year?
    time-local-utc-offset
    time-month
    time-month-start
    time-ns-current
    time-seconds-day
    time-seconds-hour
    time-seconds-minute
    time-seconds-week
    time-week-first
    time-week-start
    time-year
    time-year-start
    time-year-weeks-53?)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-19))

  ;time as integers.
  ;seconds: tai seconds since 1970-01-01 00:00:00 UTC.
  ;year/month/day in gregorian calender utc
  ;
  (define (time-from-date a) (time-second (date->time-tai a)))
  (define (time->date a) (time-tai->date (make-time time-tai 0 a)))
  (define time-seconds-minute 60)
  (define time-seconds-hour 3600)
  (define time-seconds-day 86400)
  (define time-seconds-week 604800)
  (define (time-current) (time-second (current-time (q time-tai))))

  (define (time-ns-current)
    (let (a (current-time (q time-tai))) (+ (* (time-second a) 1000000000) (time-nanosecond a))))

  (define (time->day a)
    "integer -> integer
    day of the month 1-31"
    (date-day (time->date a)))

  (define (time-day a) (- a (time-day-start a)))
  (define (time-year a) (- a (time-year-start a)))
  (define (time-month a) (- a (time-month-start a)))
  (define (time-from-year a) (time-from-date (make-date 0 0 0 0 1 1 a 0)))
  (define (time->month a) (date-month (time->date a)))
  (define (time->year a) (date-year (time->date a)))
  (define (time-days->seconds a) (* time-seconds-day a))

  (define (time-day-start a)
    (let (a (time->date a))
      (time-from-date (make-date 0 0 0 0 (date-day a) (date-month a) (date-year a) 0))))

  (define (time-week-start a) (- a (* (time->week-day a) time-seconds-day)))

  (define (time-month-start a)
    (let (a (time->date a)) (time-from-date (make-date 0 0 0 0 1 (date-month a) (date-year a) 0))))

  (define (time-year-start a)
    (let (a (time->date a)) (time-from-date (make-date 0 0 0 0 1 1 (date-year a) 0))))

  (define (time-week-first a)
    ;based on if thursday falls into the first week-days of the year
    (let* ((year-start (time-year-start a)) (week-day (time->week-day year-start)))
      (if (< week-day 4) (- year-start (+ 1 (time-days->seconds week-day)))
        (+ year-start (time-days->seconds (- 7 week-day))))))

  (define (time-year-weeks-53? a) "year with 53 iso weeks?"
    (let*
      ( (year-start-date (time->date (time-year-start a)))
        (week-day (date-week-day year-start-date)))
      ;date-week-day counts from sunday
      (or (= 4 week-day) (and (= 3 week-day) (time-leap-year-number? (date-year year-start-date))))))

  (define (time->week a)
    (let (difference (- a (time-week-first a)))
      (if (= 0 difference) 1
        (if (< difference 0) (if (time-year-weeks-53? (time-from-year (- (time->year a) 1))) 53 52)
          (let (weeks (/ difference time-seconds-week))
            ;any full week difference means the week has passed
            (if (= 0 (modulo difference time-seconds-week)) (+ 1 weeks) (ceiling weeks)))))))

  (define (time-local-utc-offset) "offset of the current local time zone to UTC"
    (* (date-zone-offset (current-date)) time-seconds-hour))

  (define (time->week-day a) "from 0-6, with monday being the first day of the week"
    (let (week-day (date-week-day (time->date a))) (if (= 0 week-day) 6 (- week-day 1))))

  (define (time-leap-year? a) (time-leap-year-number? (time->year a)))

  (define (time-leap-year-number? a)
    (or (and (= 0 (modulo a 4)) (not (= 0 (modulo a 100)))) (= 0 (modulo a 400))))

  (define* (time-from-hms hours minutes seconds) "integer ... -> integer"
    (+ (* time-seconds-hour hours) (* time-seconds-minute minutes) seconds))

  (define* (time->hms a #:optional (c list))
    "integer [procedure:{hour minute second} -> any] -> (integer integer integer)"
    (let*
      ( (hours (truncate (/ a 3600))) (hour-seconds (* 3600 hours))
        (minutes (inexact->exact (truncate (/ (- a hour-seconds) 60)))))
      (c (inexact->exact hours) minutes (inexact->exact (- a hour-seconds (* minutes 60))))))

  (define*
    (time-from-ymdhms #:key (year 0) (month 1) (day 1) (hour 0) (minute 0) (second 0)
      (offset-hour 0)
      (offset-minute 0))
    (time-from-date
      (make-date 0 second minute hour day month year (time-from-hms offset-hour offset-minute 0)))))
