(library (sph time utc)
  (export
    utc-leap-seconds
    utc-nanoseconds-day
    utc-seconds-day
    utc-seconds-hour
    utc-seconds-minute
    utc-tai->leap-second-difference)
  (import
    (rnrs base)
    (sph))

  (define utc-seconds-minute 60)
  (define utc-seconds-hour 3600)
  (define utc-seconds-day 86400)
  (define utc-nanoseconds-day 86400000000000)

  (define-as utc-leap-seconds ql
    (1341100800 . 35) (1230768000 . 34)
    (1136073600 . 33) (915148800 . 32)
    (867715200 . 31) (820454400 . 30)
    (773020800 . 29) (741484800 . 28)
    (709948800 . 27) (662688000 . 26)
    (631152000 . 25) (567993600 . 24)
    (489024000 . 23) (425865600 . 22)
    (394329600 . 21) (362793600 . 20)
    (315532800 . 19) (283996800 . 18)
    (252460800 . 17) (220924800 . 16)
    (189302400 . 15) (157766400 . 14)
    (126230400 . 13) (94694400 . 12) (78796800 . 11) (63072000 . 10))

  (define (utc-tai->leap-second-difference a)
    (if (< a (* (- 1972 1970) 365 utc-seconds-day)) 0
      (let loop ((rest utc-leap-seconds))
        (if (null? rest) 0 (let (b (first rest)) (if (>= a (first b)) (tail b) (loop (tail rest)))))))))
