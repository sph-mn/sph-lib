(library (sph time)
  (export
    current-day-seconds-string
    current-local-datetime-string
    current-local-iso-date-string
    current-local-time
    current-time-microseconds
    current-time-zone-utc-offset
    daylight-savings-time?
    seconds->datetime-string
    seconds->day
    seconds->day-of-week
    seconds->day-seconds
    seconds->day-seconds-string
    seconds->day-start
    seconds->iso-date-string
    seconds->month
    seconds->month-seconds
    seconds->month-start
    seconds->year
    seconds->year-seconds
    seconds->year-start
    traditional-time-string->seconds)
  (import
    (guile)
    (rnrs base)
    (sph)
    (only (sph math) round-to-decimal-places simple-format-number->string))

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

  (define* (seconds->day-seconds-string a #:optional (shift 2) (decimal-places 0))
    (simple-format-number->string (seconds->day-seconds a) shift decimal-places))

  (define* (current-day-seconds-string #:optional (shift 2) (decimal-places 0))
    "defaults to local hectoseconds" (seconds->day-seconds-string (current-local-time)))

  (define (seconds->datetime-string a) "iso-date with appended day-hectoseconds infixed with :"
    (string-append (seconds->iso-date-string a) ":"
      (simple-format-number->string (seconds->day-seconds a) 2)))

  (define (current-local-datetime-string) (seconds->datetime-string (current-local-time)))
  (define (current-local-iso-date-string) (seconds->iso-date-string (current-local-time)))

  (define (traditional-time-string->seconds a)
    "string -> integer
    converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0)) (+ (* hours 3600) (* minutes 60) seconds))
      (map string->number (string-split a #\:)))))