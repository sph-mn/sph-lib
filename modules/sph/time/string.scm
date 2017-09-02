(library (sph time string)
  (export
    military-time->hm
    seconds-from-hms
    sph-time-string-description
    utc->ymd
    utc->ymd-ks
    utc->ymd-s
    utc-current-ymd
    utc-current-ymd-ks
    utc-current-ymd-s
    utc-elapsed-day-string
    utc-from-military-time
    utc-from-ymd)
  (import
    (sph)
    (sph string)
    (sph time)
    (sph time utc)
    (only (guile) string-split)
    (only (sph number) simple-format-number))

  (define sph-time-string-description
    "time string conversions.
     ymd: iso8601 date, yyyy-mm-dd
     hms: hh:mm:ss")

  ;-- reading

  (define (time-from-military-time a)
    (let (a (military-time->hm a)) (+ (* 3600 (first a)) (* 60 (tail a)))))

  (define (utc-from-ymd a)
    (utc-from-date (apply date-create* (map string->number (string-split a #\-)))))

  (define (seconds-from-hms a)
    "string -> integer
     converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0))
        (seconds->nanoseconds (utc-duration-from-hms hours minutes seconds)))
      (map string->number (string-split a #\:))))

  ;-- writing

  (define* (utc-elapsed-day-string a #:optional (shift 3) (decimal-places 2))
    (simple-format-number (nanoseconds->seconds (utc-elapsed-day a)) shift decimal-places))

  (define (military-time->hm a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (utc->ymd a) "integer -> string"
    (let (date (utc->date a))
      (string-append (number->string (date-year date)) "-"
        (string-fill-left (number->string (date-month date)) 2 #\0) "-"
        (string-fill-left (number->string (date-day date)) 2 #\0))))

  (define (utc->ymd-ks a) (string-append (utc->ymd a) "_" (utc-elapsed-day-string a 3 2)))
  (define (utc->ymd-s a) (string-append (utc->ymd a) "_" (utc-elapsed-day-string a 0 0)))
  (define (utc-current-ymd) (utc->ymd (utc-current)))
  (define (utc-current-ymd-ks) (utc->ymd-ks (utc-current)))
  (define (utc-current-ymd-s) (utc->ymd-s (utc-current))))
