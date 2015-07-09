(library (sph time)
  (export
    current-datetime-string
    current-day-seconds-string
    current-iso-date-string
    current-local-datetime-string
    current-local-iso-date-string
    current-local-time
    current-time-microseconds
    current-time-zone-utc-offset
    daylight-savings-time?
    iso-8601-date+time->seconds
    seconds->datetime-string
    seconds->day
    seconds->day-of-week
    seconds->day-seconds
    seconds->day-seconds-string
    seconds->day-start
    seconds->formatted-date-string
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
    (only (sph math) round-to-decimal-places simple-format-number))

  ;epoch is 1970-01-01 00:00:00 UTC, excluding leap seconds.

  (define (parse-military-time a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (parse-military-time->seconds a)
    (let (a (parse-military-time a)) (+ (* 3600 (first a)) (* 60 (tail a)))))

  (define (iso-8601-date+time->seconds a)
    "string:\"yyyy-mm-ddThh:mm:ss+hhmm\" -> integer:posix-epoch-seconds"
    ;according to guile documentation, strptime does not set the time zone offset usefully.
    (and-let*
      ( (time (false-if-exception (first (strptime "%Y-%m-%dT%H:%M:%S%z" a))))
        (offset (string-take-right a 4)) (offset-sign (string-ref a (- (string-length a) 5))))
      (set-tm:gmtoff time
        (* (if (eqv? #\- offset-sign) -1 1) (parse-military-time->seconds offset)))
      (false-if-exception (string->number (strftime "%s" time)))))

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

  (define (seconds->datetime-string a) "iso-date with appended day-kiloseconds infixed with :"
    (string-append (seconds->iso-date-string a) ":" (seconds->day-seconds-string a)))

  (define (seconds->formatted-date-string strftime-format a) (strftime strftime-format (gmtime a)))
  (define (current-local-datetime-string) (seconds->datetime-string (current-local-time)))
  (define (current-datetime-string) (seconds->datetime-string (current-time)))
  (define (current-iso-date-string) (seconds->iso-date-string (current-time)))
  (define (current-local-iso-date-string) (seconds->iso-date-string (current-local-time)))

  (define (traditional-time-string->seconds a)
    "string -> integer
    converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0)) (+ (* hours 3600) (* minutes 60) seconds))
      (map string->number (string-split a #\:)))))