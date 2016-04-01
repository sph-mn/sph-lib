(library (sph time)
  (export
    current-datetime-string
    current-day-seconds
    current-day-seconds-string
    current-iso-date-string
    current-local-datetime-string
    current-local-iso-date-string
    current-local-time
    current-time-microseconds
    current-time-zone-utc-offset
    daylight-savings-time?
    hours-minutes-seconds->seconds
    seconds->datetime-string
    seconds->day
    seconds->day-of-week
    seconds->day-seconds
    seconds->day-seconds-string
    seconds->day-start
    seconds->formatted-date-string
    seconds->hours-minutes-seconds
    seconds->iso-date-string
    seconds->month
    seconds->month-seconds
    seconds->month-start
    seconds->year
    seconds->year-seconds
    seconds->year-start
    time-traditional-parts->seconds
    time-traditional-string->seconds)
  (import
    (guile)
    (ice-9 regex)
    (rnrs base)
    (sph)
    (only (sph number) round-to-decimal-places simple-format-number)
    (only (srfi srfi-19)
      make-date
      date->time-utc
      time-second
      time-nanosecond))

  ;epoch is 1970-01-01 00:00:00 UTC, excluding leap seconds.

  (define (parse-military-time a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (parse-military-time->seconds a)
    (let (a (parse-military-time a)) (+ (* 3600 (first a)) (* 60 (tail a)))))

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

  (define (seconds->day a) (tm:mday (gmtime a)))
  (define (seconds->month a) (tm:mon (gmtime a)))

  (define (seconds->day-of-week a) "from 1-7, with monday being the first day of the week"
    (let (wd (tm:wday (gmtime a))) (if (= 0 wd) 7 wd)))

  (define (seconds->year a) (+ 1900 (tm:year (gmtime a))))

  (define (seconds->day-start a)
    (let (t (gmtime a)) (set-tm:hour t 0)
      (set-tm:min t 0) (set-tm:sec t 0) (first (mktime t "UTC"))))

  (define (seconds->month-start a)
    (let (t (gmtime a)) (set-tm:hour t 0)
      (set-tm:min t 0) (set-tm:sec t 0) (set-tm:mday t 1) (first (mktime t "UTC"))))

  (define (seconds->year-start a)
    (let (t (gmtime a)) (set-tm:hour t 0)
      (set-tm:min t 0) (set-tm:sec t 0) (set-tm:mon t 0) (set-tm:mday t 1) (first (mktime t "UTC"))))

  (define (seconds->day-seconds a) (- a (seconds->day-start a)))
  (define (seconds->year-seconds a) (- a (seconds->year-start a)))
  (define (seconds->month-seconds a) (- a (seconds->month-start a)))

  (define (seconds->iso-date-string a)
    (let (t (gmtime a))
      (string-append (number->string (+ 1900 (tm:year t))) "-"
        (number->string (+ 1 (tm:mon t))) "-" (number->string (tm:mday t)))))

  (define* (seconds->day-seconds-string a #:optional (shift 3) (decimal-places 2))
    (simple-format-number (seconds->day-seconds a) shift decimal-places))

  (define* (current-day-seconds-string #:optional (shift 2) (decimal-places 0))
    "defaults to local hectoseconds" (seconds->day-seconds-string (current-time)))

  (define (current-day-seconds) (seconds->day-seconds (current-time)))

  (define (seconds->datetime-string a) "iso-date with appended day-kiloseconds infixed with :"
    (string-append (seconds->iso-date-string a) ":" (seconds->day-seconds-string a)))

  (define (seconds->formatted-date-string strftime-format a) (strftime strftime-format (gmtime a)))
  (define (current-local-datetime-string) (seconds->datetime-string (current-local-time)))
  (define (current-datetime-string) (seconds->datetime-string (current-time)))
  (define (current-iso-date-string) (seconds->iso-date-string (current-time)))
  (define (current-local-iso-date-string) (seconds->iso-date-string (current-local-time)))

  (define* (hours-minutes-seconds->seconds hours #:optional (minutes 0) (seconds 0))
    "integer ... -> integer" (+ (* 3600 hours) (* 60 minutes) seconds))

  (define (seconds->hours-minutes-seconds a) "integer -> (integer integer integer)"
    (let*
      ( (hours (truncate (/ a 3600))) (hour-seconds (* 3600 hours))
        (minutes (truncate (/ (- a hour-seconds) 60))))
      (list hours minutes (- a hour-seconds (* minutes 60)))))

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
      (time-second date-object)))

  (define (time-traditional-string->seconds a)
    "string -> integer
    converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0)) (+ (* hours 3600) (* minutes 60) seconds))
      (map string->number (string-split a #\:)))))
