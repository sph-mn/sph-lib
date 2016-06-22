(library (sph time)
  (export
    time->date
    time->days
    time->utc
    time-current
    time-from-utc
    time-leap-year-number?)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph one)
    (sph record))

  (define (time-seconds->nanoseconds a) (* 1000000000 a))
  (define (time-nanoseconds->seconds a) (floor (/ a 1000000000)))
  (define time-seconds-minute 60)
  (define time-seconds-hour 3600)
  (define time-seconds-day 86400)
  (define time-nanoseconds-day (time-seconds->nanoseconds 86400))
  (define-as time-month-days vector 31 28 31 30 31 30 31 31 30 31 30 31)
  (define-as time-month-days-leap-year vector 31 29 31 30 31 30 31 31 30 31 30 31)
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
    (and (= 0 (modulo a 4)) (or (not (= 0 (modulo a 100))) (= 0 (modulo a 400)))))

  (define (time-current)
    (let* ((a (gettimeofday)) (seconds (first a)) (microseconds (tail a)))
      (* 1000 (+ (* seconds 1000000) microseconds))))

  (define (time->utc a) (- a (time-seconds->nanoseconds (tai->utc-leap-second-difference a))))
  (define (time->days a) (/ (time->utc a) time-nanoseconds-day))

  (define (time-from-utc a)
    ;todo: check if the leap-second difference selection makes sense for utc->tai
    (+ a (time-seconds->nanoseconds (tai->utc-leap-second-difference a))))

  (define (time-years->days a) (- (+ (* a 365) (quotient a 4) (quotient a 400)) (quotient a 100)))
  (define years-400-days 146097)
  (define years-4-days 1461)
  (define years-100-days 36524)
  (define year-days 365.2425)

  (define (time-days->year a)
    (/ a years-400-days)
    (truncate
      (/
        (+ (- a (quotient a years-400-days) (quotient a years-4-days)) (quotient a years-100-days))
        365)))

  (define (year-days->month-index-and-rest& a time-month-days c)
    (let loop ((index 0) (days 0))
      (if (< index (vector-length time-month-days))
        (let (days (+ days (vector-ref time-month-days index)))
          (if (< a days) (c index (- days a)) (loop (+ 1 index) days)))
        (c #f #f))))

  (define (time->date a)
    (let*
      ( (days (+ (time-years->days 1970) (time->days a))) (year (time-days->year (truncate days)))
        (year-rest (- days (time-years->days year))))
      (year-days->month-index-and-rest& year-rest
        (if (time-leap-year-number? year) time-month-days-leap-year time-month-days)
        (l (month-index month-rest) (list year (+ 1 month-index) (truncate month-rest)))))))
