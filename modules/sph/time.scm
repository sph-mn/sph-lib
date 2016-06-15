(library (sph time)
  (export
    current-day-seconds
    current-local-time
    current-time-microseconds
    current-time-zone-utc-offset
    daylight-savings-time?
    days->seconds
    hours-minutes-seconds->seconds
    leap-year?
    seconds->day
    seconds->day-seconds
    seconds->day-start
    seconds->extra-week-year?
    seconds->first-week-start
    seconds->hours-minutes-seconds
    seconds->month
    seconds->month-seconds
    seconds->month-start
    seconds->week-day
    seconds->week-number
    seconds->year
    seconds->year-seconds
    seconds->year-start
    seconds-day
    seconds->date
    date->seconds
    seconds-hour
    seconds-leap-year?
    seconds-minute
    seconds-week
    time-traditional-parts->seconds
    year->seconds)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-19))

  ;epoch is 1970-01-01 00:00:00 UTC, excluding leap seconds.

  (define (current-time-zone-utc-offset)
    "offset of the current time zone to UTC without daylight-savings-time"
    (let (local-time (localtime (current-time)))
      (- (* -1 (tm:gmtoff local-time)) (if (> (tm:isdst local-time) 0) 3600 0))))

  (define (daylight-savings-time?) (> (tm:isdst (localtime (current-time))) 0))

  (define (current-local-time) "seconds since unix time epoch in the current time zone"
    (+ (current-time-zone-utc-offset) (current-time)))

  (define (current-time-microseconds)
    "-> integer
    return the number of microseconds since 1970-01-01 00:00:00 UTC, excluding leap seconds."
    ((l (r) (+ (* (first r) 1000000) (tail r))) (gettimeofday)))

  (define (seconds->thursday-difference a) (days->seconds (- 3 (seconds->week-day a))))

  (define (seconds-extra-week-year? a) "year with 53 iso weeks?"
    (let*
      ( (year-start-date (seconds->date (seconds->year-start a)))
        (week-day (date-week-day year-start-date)))
      (or (= 3 week-day) (and (= 2 week-day) (leap-year? (date-year year-start-date))))))

  (define (seconds->week-number a) "iso week number"
    (let*
      ( (a-thursday (+ a (seconds->thursday-difference a)))
        (first-week-start (seconds->first-week-start a))
        (first-thursday (+ first-week-start (seconds->thursday-difference first-week-start)))
        (difference (- a-thursday first-thursday)))
      (if (< difference 0) (if (seconds-extra-week-year? (- (seconds->year a) 1)) 53 52)
        (+ 1 (ceiling (/ difference seconds-week))))))

  (define (date->seconds a) (time-second (date->time-utc a)))
  (define (seconds->date a) (time-utc->date (make-time time-utc 0 a)))
  (define (seconds->day a) "1-31" (date-day (seconds->date a)))
  (define (seconds->month a) (date-month (seconds->date a)))
  (define (seconds->year a) (date-year (seconds->date a)))
  (define (days->seconds a) (* seconds-day a))
  (define seconds-minute 60)
  (define seconds-hour 60)
  (define seconds-day 86400)
  (define seconds-week (* 7 seconds-day))

  (define (seconds->first-week-start a)
    ;based on if thursday falls into the first week-days of the year
    (let* ((year-start (seconds->year-start a)) (week-day (seconds->week-day year-start)))
      (if (< week-day 4) (- year-start (+ 1 (days->seconds week-day)))
        (+ year-start (days->seconds (- 7 week-day))))))

  (define (seconds->week-day a) "from 0-6, with monday being the first day of the week"
    (let (week-day (date-week-day (seconds->date a))) (if (= 0 week-day) 6 (- week-day 1))))

  (define (seconds->day-start a)
    (let (a (seconds->date a))
      (date->seconds (make-date 0 0 0 0 (date-day a) (date-month a) (date-year a) 0))))

  (define (seconds->month-start a)
    (let (a (seconds->date a)) (time-second (make-date 0 0 0 0 1 (date-month a) (date-year a) 0))))

  (define (seconds->year-start a)
    (let (a (seconds->date a)) (date->seconds (make-date 0 0 0 0 1 1 (date-year a) 0))))

  (define (seconds-leap-year? a) (leap-year? (seconds->year a)))

  (define (leap-year? a)
    (or (and (= 0 (modulo a 4)) (not (= 0 (modulo a 100)))) (= 0 (modulo a 400))))

  (define (year->seconds a) (let (a (make-date 0 0 0 0 1 1 a 0)) (date->seconds a)))
  (define (seconds->day-seconds a) (- a (seconds->day-start a)))
  (define (seconds->year-seconds a) (- a (seconds->year-start a)))
  (define (seconds->month-seconds a) (- a (seconds->month-start a)))
  (define (current-day-seconds) (seconds->day-seconds (current-time)))

  (define* (hours-minutes-seconds->seconds hours #:optional (minutes 0) (seconds 0))
    "integer ... -> integer" (+ (* 3600 hours) (* 60 minutes) seconds))

  (define (seconds->hours-minutes-seconds a) "integer -> (integer integer integer)"
    (let*
      ( (hours (truncate (/ a 3600))) (hour-seconds (* 3600 hours))
        (minutes (inexact->exact (truncate (/ (- a hour-seconds) 60)))))
      (list (inexact->exact hours) minutes (inexact->exact (- a hour-seconds (* minutes 60))))))

  (define*
    (time-traditional-parts->seconds #:key (year 0) (month 0) (day 0) (hours 0) (minutes 0)
      (seconds 0)
      (offset-hours 0)
      (offset-minutes 0))
    "integer ... -> (seconds . nanoseconds)
    create the posix-time seconds corresponding to the set of given traditionally used time units.
    parts default to zero. offset-hours and offset-minutes may be negative"
    (let
      (date-object
        (date->time-utc
          (make-date 0 seconds
            minutes hours day month year (hours-minutes-seconds->seconds offset-hours offset-minutes))))
      (time-second date-object))))
