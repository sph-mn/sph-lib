(library (sph time utc)
  (export
    utc-duration->hms
    utc-duration-from-hms
    utc-leap-second?
    utc-leap-seconds
    utc-nanoseconds-day
    utc-nanoseconds-hour
    utc-nanoseconds-minute
    utc-nanoseconds-week
    utc-seconds-day
    utc-tai->leap-second-difference)
  (import
    (rnrs base)
    (sph)
    (only (guile) truncate/ assoc-ref))

  (define utc-nanoseconds-minute 60000000000)
  (define utc-nanoseconds-hour 3600000000000)
  (define utc-nanoseconds-day 86400000000000)
  (define utc-nanoseconds-week (* 7 utc-nanoseconds-day))
  (define utc-seconds-day 86400)

  (define-as utc-leap-seconds ql
    (1435708800 . 36) (1341100800 . 35)
    (1230768000 . 34) (1136073600 . 33)
    (915148800 . 32) (867715200 . 31)
    (820454400 . 30) (773020800 . 29)
    (741484800 . 28) (709948800 . 27)
    (662688000 . 26) (631152000 . 25)
    (567993600 . 24) (489024000 . 23)
    (425865600 . 22) (394329600 . 21)
    (362793600 . 20) (315532800 . 19)
    (283996800 . 18) (252460800 . 17)
    (220924800 . 16) (189302400 . 15)
    (157766400 . 14) (126230400 . 13) (94694400 . 12) (78796800 . 11) (63072000 . 10))

  (define (utc-leap-second? a) (integer? (assoc-ref utc-leap-seconds a)))

  (define (utc-tai->leap-second-difference a)
    (if (< a 63072000) 0
      (let loop ((rest utc-leap-seconds))
        (if (null? rest) 0 (let (b (first rest)) (if (>= a (first b)) (tail b) (loop (tail rest))))))))

  (define* (utc-duration-from-hms hours minutes seconds)
    "integer ... -> integer
    utc duration are seconds from zero, unrelated to the unix epoch"
    (+ (* 3600 hours) (* 60 minutes) seconds))

  (define* (utc-duration->hms a #:optional (c list))
    "integer [procedure:{hour minute second} -> any] -> (integer integer integer)"
    (apply-values
      (l (hours rest)
        (apply-values (l (minutes seconds) (c hours minutes seconds)) (truncate/ rest 60)))
      (truncate/ a 3600))))
