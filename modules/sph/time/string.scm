(library (sph time string)
  (export
    current-datetime-string
    current-day-seconds-string
    current-iso-date-string
    current-local-datetime-string
    current-local-iso-date-string
    seconds->datetime-string
    seconds->day-seconds-string
    seconds->formatted-date-string
    seconds->iso-date-string
    time-traditional-string->seconds)
  (import
    (ice-9 regex)
    (rnrs base)
    (sph)
    (only (sph number) round-to-decimal-places simple-format-number))

  (define (parse-military-time a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (parse-military-time->seconds a)
    (let (a (parse-military-time a)) (+ (* 3600 (first a)) (* 60 (tail a)))))

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

  (define (time-traditional-string->seconds a)
    "string -> integer
    converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0)) (+ (* hours 3600) (* minutes 60) seconds))
      (map string->number (string-split a #\:)))))
