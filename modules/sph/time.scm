(library (sph time)
  (export
    time->day
    time->month
    time->week
    time->week-day
    time->year
    time-current
    time-day
    time-day-start
    time-days->seconds
    time-from-month
    time-from-week
    time-from-year
    time-hms->seconds
    time-leap-year?
    time-local-utc-offset
    time-month-start
    time-seconds-day
    time-seconds-hour
    time-seconds-minute
    time-seconds-week
    time-week-first
    time-week-start
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
  (define (date->time a) (time-second (date->time-tai a)))
  (define (time->date a) (time-tai->date (make-time time-tai 0 a)))
  (define time-seconds-minute 60)
  (define time-seconds-hour 3600)
  (define time-seconds-day 86400)
  (define time-seconds-week 604800)

  (define (time->day a)
    "integer -> integer
    day of the month 1-31"
    (date-day (time->date a)))

  (define (time-from-year a) (date->time (make-date 0 0 0 0 1 1 a 0)))
  (define (time->month a) (date-month (time->date a)))
  (define (time->year a) (date-year (time->date a)))
  (define (time-days->seconds a) (* time-seconds-day a))

  (define (time-day-start a)
    (let (a (time->date a))
      (date->time (make-date 0 0 0 0 (date-day a) (date-month a) (date-year a) 0))))

  (define (time-week-start a) (- a (* (time->week-day a) time-seconds-day)))

  (define (time-month-start a)
    (let (a (time->date a)) (date->time (make-date 0 0 0 0 1 (date-month a) (date-year a) 0))))

  (define (time-year-start a)
    (let (a (time->date a)) (date->time (make-date 0 0 0 0 1 1 (date-year a) 0))))

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
      (or (= 4 week-day) (and (= 3 week-day) (leap-year? (date-year year-start-date))))))

  (define (time->week a)
    (let (difference (- a (time-week-first a)))
      (if (= 0 difference) 1
        (if (< difference 0) (if (time-year-weeks-53? (time-from-year (- (time->year a) 1))) 53 52)
          (ceiling (/ difference time-seconds-week))))))

  ;--

  (define (time-local-utc-offset) "offset of the current local time zone to UTC"
    (* (date-zone-offset (current-date)) time-seconds-hour))

  (define (time-local-current-seconds) "seconds since unix time epoch in the current time zone"
    (time-second (date->time-utc (current-time))))

  (define (time->week-day a) "from 0-6, with monday being the first day of the week"
    (let (week-day (date-week-day (time->date a))) (if (= 0 week-day) 6 (- week-day 1))))

  (define (seconds-leap-year? a) (leap-year? (time->year a)))

  (define (leap-year? a)
    (or (and (= 0 (modulo a 4)) (not (= 0 (modulo a 100)))) (= 0 (modulo a 400))))

  (define (year->seconds a) (let (a (make-date 0 0 0 0 1 1 a 0)) (date->time a)))
  (define (time->day-seconds a) (- a (time-day-start a)))
  (define (time->year-seconds a) (- a (time-year-start a)))
  (define (time->month-seconds a) (- a (time-month-start a)))
  (define (current-day-seconds) (time->day-seconds (current-time)))

  (define* (hms-time->seconds hours #:optional (minutes 0) (seconds 0)) "integer ... -> integer"
    (+ (* 3600 hours) (* 60 minutes) seconds))

  (define (time->hms a) "integer -> (integer integer integer)"
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
            minutes hours day month year (hms-time->seconds offset-hours offset-minutes))))
      (time-second date-object))))
