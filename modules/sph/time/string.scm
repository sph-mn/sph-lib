(library (sph time string)
  (export
    military-time->hm
    s->hms-string
    s->ks-string
    seconds-from-hms
    sph-time-string-description
    utc->ymd
    utc->ymd-hms
    utc->ymd-ks
    utc->ymd-s
    utc-current-ymd
    utc-current-ymd-ks
    utc-current-ymd-s
    utc-elapsed-day-string
    utc-from-military-time
    utc-from-ymd
    ymd-daytime-delimiter)
  (import
    (sph)
    (sph list)
    (sph number)
    (sph string)
    (sph time)
    (sph time utc)
    (only (guile) string-split string-join))

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
     converts a string time representation of hours:minutes:seconds,
     where minutes and seconds are optional, to seconds"
    (apply
      (l* (hours #:optional (minutes 0) (seconds 0)) (utc-duration-from-hms hours minutes seconds))
      (map string->number (string-split a #\:))))

  ;-- writing

  (define* (utc-elapsed-day-string a #:optional (scale 3) (decimal-min 0))
    (number-format-float (/ (nanoseconds->seconds (utc-elapsed-day a)) (expt 10 scale))
      #:decimal-max 2 #:decimal-min decimal-min))

  (define (military-time->hm a) "-> (hours . minutes)"
    (pair (string->number (substring a 0 2)) (string->number (substring a 2 4))))

  (define (utc->ymd a) "integer -> string"
    (let (date (utc->date a))
      (string-append (number->string (date-year date)) "-"
        (string-fill-left (number->string (date-month date)) 2 #\0) "-"
        (string-fill-left (number->string (date-day date)) 2 #\0))))

  (define ymd-daytime-delimiter "_")

  (define (utc->ymd-ks a)
    (string-append (utc->ymd a) ymd-daytime-delimiter (utc-elapsed-day-string a 3 0)))

  (define (utc->ymd-hms a)
    (string-append (utc->ymd a) ymd-daytime-delimiter (s->hms-string (ns->s (utc-elapsed-day a)))))

  (define (utc->ymd-s a)
    (string-append (utc->ymd a) ymd-daytime-delimiter (utc-elapsed-day-string a 0 0)))

  (define (utc-current-ymd) (utc->ymd (utc-current)))
  (define (utc-current-ymd-ks) (utc->ymd-ks (utc-current)))
  (define (utc-current-ymd-s) (utc->ymd-s (utc-current)))
  (define (s->ks-string a) (number-format-float (/ a 1000) #:decimal-min 0 #:decimal-max 2))

  (define* (s->hms-string a #:optional (drop null))
    (let* ((hms (utc-duration->hms (abs a))) (hms (if (null? drop) hms (list-deselect hms drop))))
      (string-append (if (> 0 a) "-" "")
        (string-join
          (map (l (a) (string-fill-left (number-format-float a #:decimal-max 0) 2 #\0)) hms) ":")))))
