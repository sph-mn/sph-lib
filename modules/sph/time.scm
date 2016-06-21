(library (sph time)
  (export
    time->days
    time->utc
    time-current
    time-from-utc
    time->date
    time-leap-year-number?)
  (import
    (rnrs base)
    (sph)
    (sph record)
    (only (guile) gettimeofday modulo))

  (define (time-seconds->nanoseconds a) (* 1000000000 a))
  (define (time-nanoseconds->seconds a) (floor (/ a 1000000000)))
  (define time-seconds-minute 60)
  (define time-seconds-hour 3600)
  (define time-seconds-day 86400)
  (define time-nanoseconds-day (time-seconds->nanoseconds 86400))

  (define-as time-month-days vector
    31
    28
    31
    30
    31
    30
    31
    31
    30
    31
    30
    31)

  (vector 1970 1 1 0 0 0 0)
  (define-record date year month day hour minute second nanosecond)

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

  (define (tai->utc-leap-second-difference a)
    (if (< a (* (- 1972 1970) 365 time-seconds-day)) 0
      (let loop ((rest utc-leap-seconds))
        (if (null? rest) 0 (let (b (first rest)) (if (>= a (first b)) (tail b) (loop (tail rest))))))))

  (define (time-leap-year-number? a)
    (or (and (= 0 (modulo a 4)) (not (= 0 (modulo a 100)))) (= 0 (modulo a 400))))

  (define (time-current)
    (let* ((a (gettimeofday)) (seconds (first a)) (microseconds (tail a)))
      (* 1000 (+ (* seconds 1000000) microseconds))))

  (define (time->utc a) (- a (time-seconds->nanoseconds (tai->utc-leap-second-difference a))))
  (define (time->days a) (/ (time->utc a) time-nanoseconds-day))
  (define (time-from-utc a)
    ;todo: check if the leap-second difference selection makes sense for utc->tai
    (+ a (time-seconds->nanoseconds (tai->utc-leap-second-difference a))))


  (define greg-year-days 365.2425)

  (define (time->date a)
    (let (days (time->days a))
      (floor (+ 1970 (/ days greg-year-days)))

      )


    )

  )
