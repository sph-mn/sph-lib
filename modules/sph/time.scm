(library (sph time)
  (export
    date->week-day
    date-add-day
    date-day
    date-hour
    date-minute
    date-month
    date-nanosecond
    date-new
    date-new*
    date-offset
    date-second
    date-week-count
    date-year
    nanoseconds->hms&
    nanoseconds->seconds
    ns->s
    s->ns
    seconds->nanoseconds
    sph-time-description
    tai->utc
    tai-add-minutes
    tai-add-seconds
    tai-current
    tai-from-utc
    utc->date
    utc->days
    utc->hours
    utc->minutes
    utc->seconds
    utc->week
    utc->week-day
    utc->year
    utc->years
    utc-add-day
    utc-add-hours
    utc-add-weeks
    utc-add-years
    utc-current
    utc-days
    utc-days-and-rest&
    utc-elapsed-day
    utc-elapsed-hour
    utc-elapsed-minute
    utc-elapsed-month
    utc-elapsed-year
    utc-from-date
    utc-from-days
    utc-from-hours
    utc-from-minutes
    utc-from-year
    utc-from-years
    utc-start-day
    utc-start-first-week
    utc-start-hour
    utc-start-last-week
    utc-start-minute
    utc-start-month
    utc-start-second
    utc-start-week
    utc-start-year
    utc-year
    utc-zone-offset)
  (import
    (guile)
    (sph)
    (sph time gregorian)
    (sph time utc)
    (only (sph vector) vector-accessor))

  (define sph-time-description
    "time as tai or utc nanoseconds since the unix epoch or gregorian calendar dates.
     get, manipulate and convert dates and times in scheme.
     uses a proleptic gregorian calendar with negative years and a year 0 equivalent to 1 BCE
     iso8601 and the gnu date utility include a year zero as well, to which the output is compatible.
     international atomic time is used because it does not use leap seconds. with utc it is
     not predictable when future leap seconds will be inserted, which makes it impossible to calculate accurate future times with utc.
     the implementation does not depend on other time libraries, only on a function that gives the current utc posixtime.
     it might also contain useful examples for calendar and time calculation implementors.
     objects
       utc: integer: utc seconds  since the unix epoch. utc uses leap seconds to conform to (= number-of-days (/ utc-seconds 86400))
       tai: integer: tai seconds since the unix epoch. as elapsed, no leap seconds
       date: vector: gregorian calendar date, daytime and timezone offset")

  (define (seconds->nanoseconds a) (* 1000000000 a))
  (define (nanoseconds->seconds a) (floor (/ a 1000000000)))
  (define ns->s nanoseconds->seconds)
  (define s->ns seconds->nanoseconds)

  (define (nanoseconds->hms& a c)
    (apply-values
      (l (hour nanoseconds-rest)
        (apply-values
          (l (minute nanoseconds-rest)
            (apply-values (l (seconds nanoseconds) (c hour minute seconds nanoseconds))
              (truncate/ nanoseconds-rest (seconds->nanoseconds 1))))
          (truncate/ nanoseconds-rest (seconds->nanoseconds 60))))
      (truncate/ a (seconds->nanoseconds 3600))))

  (define (tai-current) "current tai seconds since the unix epoch" (tai-from-utc (utc-current)))

  (define (tai->utc a)
    "integer -> integer
     convert tai time to utc time"
    (- a (seconds->nanoseconds (utc-tai->leap-second-difference (nanoseconds->seconds a)))))

  (define (tai-from-utc a)
    "integer -> integer
     convert utc time to tai time"
    (+ a (seconds->nanoseconds (utc-tai->leap-second-difference (nanoseconds->seconds a)))))

  (define (tai-add-minutes a minutes) "add minutes to tai or utc time"
    (+ (* utc-nanoseconds-minute minutes) a))

  (define (tai-add-seconds a seconds) "add seconds to tai or utc time"
    (+ (seconds->nanoseconds seconds) a))

  (define date-year (vector-accessor 1))
  (define date-month (vector-accessor 2))
  (define date-day (vector-accessor 3))
  (define date-hour (vector-accessor 4))
  (define date-minute (vector-accessor 5))
  (define date-second (vector-accessor 6))
  (define date-nanosecond (vector-accessor 7))
  (define date-offset (vector-accessor 8))

  (define*
    (date-new #:key (year 1) (month 1) (day 1) (hour 0) (minute 0) (second 0) (nanosecond 0)
      (offset 0))
    "create a date object" (vector (q date) year month day hour minute second nanosecond offset))

  (define*
    (date-new* #:optional (year 1) (month 1) (day 1) (hour 0) (minute 0) (second 0) (nanosecond 0)
      (offset 0))
    "like date-new but the arguments are not keyword arguments"
    (vector (q date) year month day hour minute second nanosecond offset))

  (define (date-week-count a) (if (greg-year-weeks-53? (date-year a)) 53 52))

  (define (date->week-day a) "from 0-6, with monday being the first day of the week"
    (greg-week-day (date-year a) (date-month a) (date-day a)))

  (define (date-add-day a)
    (let (year (date-year a))
      (let*
        ( (month (date-month a)) (day (date-day a))
          (day-count (vector-ref (greg-month-days-get (greg-year-leap-year? year)) (- month 1))))
        (date-new* (if (and (= month 12) (= day day-count)) (+ 1 year) year)
          (if (= day day-count) (+ 1 (modulo month 12)) month) (+ 1 (modulo day day-count))
          (date-hour a) (date-minute a) (date-second a) (date-nanosecond a) (date-offset a)))))

  ;-- utc
  (define utc-add-minutes tai-add-minutes)
  (define utc-add-seconds tai-add-seconds)

  (define (utc-zone-offset) "get the current local system timezone offset"
    (tm:gmtoff (localtime (current-time))))

  (define (utc-current) "-> integer"
    (let* ((a (gettimeofday)) (seconds (first a)) (microseconds (tail a)))
      (* 1000 (+ (* seconds 1000000) microseconds))))

  (define (utc-start-first-week a)
    "iso standard first week of current year of time.
     based on if thursday falls into the first week-days of the year"
    (let* ((year-start (utc-start-year a)) (week-day (utc->week-day year-start)))
      (if (< week-day 4) (- year-start (* utc-nanoseconds-day week-day))
        (+ year-start (* utc-nanoseconds-day (- 7 week-day))))))

  (define (utc-start-last-week a) "the start of the last week of the year"
    (- (utc-start-first-week (utc-add-years a 1)) utc-nanoseconds-week))

  (define (utc-start-year a) (let (a (utc->date a)) (utc-from-date (date-new* (date-year a) 1 1))))

  (define (utc-start-month a)
    (let (a (utc->date a)) (utc-from-date (date-new* (date-year a) (date-month a) 1))))

  (define (utc-start-day a) (- a (modulo a utc-nanoseconds-day)))
  (define (utc-start-hour a) (- a (modulo a utc-nanoseconds-hour)))
  (define (utc-start-minute a) (- a (modulo a utc-nanoseconds-minute)))
  (define (utc-start-second a) (- a (modulo a 1000000000)))

  (define (utc-start-week a)
    (let (a-minus-days (- a (* (utc->week-day a) utc-nanoseconds-day)))
      (utc-start-day a-minus-days)))

  (define (utc-days a) (/ a utc-nanoseconds-day))

  (define (utc-year a)
    (let (days (quotient a utc-nanoseconds-day))
      (+ 1970 (truncate (/ (- days (greg-days->leap-days days)) greg-year-days)))))

  (define (utc-from-date a)
    (+
      (* utc-nanoseconds-day
        (- (greg-years->days (greg-year->years (date-year a))) greg-year-1970-days))
      (* utc-nanoseconds-day (greg-month->days (date-month a) (greg-year-leap-year? (date-year a))))
      (* utc-nanoseconds-day (- (date-day a) 1)) (* utc-nanoseconds-hour (date-hour a))
      (* utc-nanoseconds-minute (date-minute a)) (seconds->nanoseconds (date-second a))
      (date-nanosecond a) (* (seconds->nanoseconds (date-offset a)) -1)))

  (define (utc-from-years a) (* utc-nanoseconds-day (- (greg-years->days a) greg-year-1970-days)))

  (define (utc-from-year a)
    (* utc-nanoseconds-day (+ (* a greg-year-days) (greg-years->leap-days (greg-year->years a)))))

  (define (utc-from-days a) (* utc-nanoseconds-day (- a greg-year-1970-days)))
  (define (utc-from-hours a) (* utc-nanoseconds-hour a))
  (define (utc-from-minutes a) (* utc-nanoseconds-minute a))

  (define (utc-days-and-rest& a c)
    (apply-values (l (days day-rest) (c (+ greg-year-1970-days days) day-rest))
      (truncate/ a utc-nanoseconds-day)))

  (define (utc->date a)
    (utc-days-and-rest& a
      (l (days day-rest)
        (let*
          ( (years (greg-days->years days)) (year (greg-days->year days))
            (leap-year? (greg-year-leap-year? year))
            (days (greg-days->year-days (- days (greg-years->days years)) leap-year?))
            (days-per-month (greg-month-days-get leap-year?)))
          (greg-year-days->month-and-day& days days-per-month
            (l (month month-day)
              (nanoseconds->hms& day-rest
                (l (h m s ns) (date-new* year month month-day h m s ns 0)))))))))

  (define (utc->week a) "integer -> integer"
    (let*
      ((year (utc->year a)) (first-week (utc-start-first-week a)) (difference (- a first-week)))
      (if (= 0 difference) 1
        (if (< difference 0) (if (greg-year-weeks-53? (- year 1)) 53 52)
          (let (last-week (utc-start-last-week a))
            (if (= a last-week) (if (greg-year-weeks-53? year) 53 52)
              (if (> a last-week)
                (if (>= (/ (- a last-week) utc-nanoseconds-week) 1) 1
                  (if (greg-year-weeks-53? year) 53 52))
                (let (weeks (/ difference utc-nanoseconds-week))
                  (if (integer? weeks) (+ 1 weeks) (ceiling weeks))))))))))

  (define (utc->week-day a) "from 0-6, with monday being the first day of the week"
    (let (a (utc->date a)) (greg-week-day (date-year a) (date-month a) (date-day a))))

  (define (utc->days a) (/ a utc-nanoseconds-day))
  (define (utc->years a) (greg-days->years (+ greg-year-1970-days (utc->days a))))
  (define (utc->year a) (greg-days->year (+ greg-year-1970-days (utc->days a))))
  (define (utc->hours a) (/ a utc-nanoseconds-hour))
  (define (utc->minutes a) (/ a utc-nanoseconds-minute))
  (define (utc->seconds a) (/ a 1000000000))
  (define (utc-elapsed-day a) (- a (utc-start-day a)))
  (define (utc-elapsed-year a) (- a (utc-start-year a)))
  (define (utc-elapsed-month a) (- a (utc-start-month a)))
  (define (utc-elapsed-hour a) (- a (utc-start-hour a)))
  (define (utc-elapsed-minute a) (- a (utc-start-minute a)))

  (define (utc-add-years a years)
    (* utc-nanoseconds-day (- (greg-years->days (+ years (utc->years a))) greg-year-1970-days)))

  (define (utc-add-day a days) (+ (* utc-nanoseconds-day days) a))
  (define (utc-add-hours a hours) (+ (* utc-nanoseconds-hour hours) a))
  (define (utc-add-weeks a weeks) (+ (* utc-nanoseconds-day weeks 7) a)))
