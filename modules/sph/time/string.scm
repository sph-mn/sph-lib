(library (sph time string)
  (export
    military-time->hm
    sph-time-string-description
    time->ymd
    time->ymd-ks
    time->ymd-s
    time-current-ymd
    time-current-ymd-ks
    time-current-ymd-s
    time-elapsed-day-string
    time-from-hms
    time-from-military-time
    time-from-ymd)
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

  (define (time-from-ymd a)
    (time-from-date (apply time-make-date* (map string->number (string-split a #\-)))))

  (define (time-from-hms a)
    "string -> integer
     converts a string time representation of hours:minutes:seconds, where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0))
        (time-seconds->nanoseconds (utc-duration-from-hms hours minutes seconds)))
      (map string->number (string-split a #\:))))

  ;-- writing

  (define* (time-elapsed-day-string a #:optional (shift 3) (decimal-places 2))
    (simple-format-number (time-nanoseconds->seconds (time-elapsed-day a)) shift decimal-places))

  (define (military-time->hm a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (time->ymd a) "integer -> string"
    (let (date (time->date a))
      (string-append (number->string (time-date-year date)) "-"
        (string-fill-left (number->string (time-date-month date)) 2 #\0) "-"
        (string-fill-left (number->string (time-date-day date)) 2 #\0))))

  (define (time->ymd-ks a) (string-append (time->ymd a) "_" (time-elapsed-day-string a 0 0)))
  (define (time->ymd-s a) (string-append (time->ymd a) "_" (time-elapsed-day-string a 0 0)))
  (define (time-current-ymd) (time->ymd (time-current)))
  (define (time-current-ymd-ks) (time->ymd-ks (time-current)))
  (define (time-current-ymd-s) (time->ymd-s (time-current))))
