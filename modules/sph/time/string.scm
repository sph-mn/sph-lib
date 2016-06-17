(library (sph time string)
  (export
    time->iso8601-ks-string
    time->iso8601-ymd
    time-current-iso8601-ks-string
    time-current-iso8601-ymd
    time-day-string
    time-from-hms-string
    time-from-military-time)
  (import
    (rnrs base)
    (sph)
    (sph time)
    (srfi srfi-19)
    (only (guile) string-split)
    (only (sph number) simple-format-number))

  (define (military-time->hm a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (time-from-military-time a)
    (let (a (military-time->hm a)) (+ (* 3600 (first a)) (* 60 (tail a)))))

  (define (time->iso8601-ymd a)
    (let (date (time->date a))
      (string-append (number->string (date-year date)) "-"
        (number->string (date-month date)) "-" (number->string (date-day date)))))

  (define* (time-day-string a #:optional (shift 3) (decimal-places 2))
    (simple-format-number (time-day a) shift decimal-places))

  (define (time-current-iso8601-ymd) (time->iso8601-ymd (time-current)))

  (define (time->iso8601-ks-string a)
    (string-append (time->iso8601-ymd a) ":" (time-day-string a)))

  (define (time-current-iso8601-ks-string) (time->iso8601-ks-string (time-current)))

  (define (time-from-hms-string a)
    "string -> integer
    converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply (l* (hours #:optional (minutes 0) (seconds 0)) (time-from-hms hours minutes seconds))
      (map string->number (string-split a #\:)))))
