(library (sph time gregorian)
  (export
    greg-days->leap-days
    greg-days->year
    greg-days->year-days
    greg-days->years
    greg-month->days
    greg-month-days
    greg-month-days-get
    greg-month-days-leap-year
    greg-number-of-months
    greg-week-day
    greg-year->years
    greg-year-days
    greg-year-days->month-and-day&
    greg-year-days-leap-year
    greg-year-first-week-day
    greg-year-leap-year?
    greg-year-weeks-53?
    greg-years->days
    greg-years->leap-days
    greg-years->year)
  (import
    (guile)
    (rnrs base)
    (sph)
    (only (guile)
      truncate-quotient
      modulo
      truncate/)
    (only (sph one) apply-values))

  ;iso8601 is supposed to be followed, which uses a year 0. a year 0 keeps leap-day calculations simpler
  (define-as greg-month-days vector 31 28 31 30 31 30 31 31 30 31 30 31)
  (define-as greg-month-days-leap-year vector 31 29 31 30 31 30 31 31 30 31 30 31)
  (define greg-number-of-months 12)
  (define greg-year-days 365)
  (define greg-year-days-leap-year 366)
  ;days in years including leap years
  (define years-400-days 146096)
  (define month-2-29-days 60)
  (define years-3-month-2-29-days (+ (* 3 greg-year-days) month-2-29-days))
  (define years-4-days 1461)
  (define years-100-days 36524)

  (define (greg-years->year a)
    "does not work reliably when year is negative and part of a date with advanced months or days,
    because advancing days in a negative year reduce the number of elapsed years but not the year number"
    (+ a 1))

  (define (greg-year->years a) (- a 1))

  (define (greg-years->leap-days a)
    "integer -> integer
    the number of leap days that occured when given years have elapsed from the first day of the calendar.
    negative values for negative years
    year 0 is a leap year and begins after -1 years. the fifth negative year completes a leap year. the fourth year completes a new year. "
    (let
      (r
        (let (a (abs (if (negative? a) (+ a 1) a)))
          (- (truncate-quotient a 4) (- (truncate-quotient a 100) (truncate-quotient a 400)))))
      (if (negative? a) (+ 1 r) r)))

  (define (greg-years->days a)
    "integer -> integer
    gives the days contained in given number of fully elapsed years"
    ((if (negative? a) - +) (* a greg-year-days) (greg-years->leap-days a)))

  (define (greg-days->year-days a leap-year?)
    "gives the number of days elapsed in a year. handles negative years/days"
    (if (negative? a) (+ (if leap-year? greg-year-days-leap-year greg-year-days) a) a))

  (define (greg-days->leap-days a)
    "integer -> integer
    gives the number of leap days in a given time span of full days"
    ;the leap year cycle is 400 years long and contains 97 leap days.
    ;the following counts cycles from bigger to smaller while subtracting
    ;the days of the matched cycles before continuing with the next step.
    ;since the given value is in days, partial years/cycles are relevant
    (apply-values
      (l (cycles-400 rest-400)
        (apply-values
          (l (cycles-100 rest-100)
            (apply-values
              (l (cycles-4 rest-4)
                (debug-log rest-4 rest-100 years-100-days (- years-100-days (abs rest-100)) years-4-days)
                (+ (* (abs cycles-400) 97) (* (abs cycles-100) 24)
                  (abs cycles-4)
                  (if (negative? a)
                    ;check if the last day falls into a centurial year.
                    ;if true, no partial year has to be considered
                    (if (< (- years-100-days (abs rest-100)) years-4-days) 0
                      ;check for partial years that have passed the leap year day 2-29
                      (if (>= (abs rest-4) (- greg-year-days month-2-29-days)) 1 0))
                    (if (< (- years-100-days rest-100) years-4-days) 0
                      (if (< rest-4 years-3-month-2-29-days) 0 1)))))
              (truncate/ rest-100 years-4-days)))
          (truncate/ rest-400 years-100-days)))
      (truncate/ a years-400-days)))

  (define (greg-days->years a)
    "integer -> integer
    the number of years the given number days fill completely"
    (truncate-quotient ((if (negative? a) + -) a (greg-days->leap-days a)) greg-year-days))

  (define (greg-days->year a)
    ;floor is the largest integer less than or equal to x
    (let (years (/ ((if (negative? a) + -) a (greg-days->leap-days a)) greg-year-days))
      (if (and (negative? a) (zero? years)) 0 (greg-years->year (floor years)))))

  (define (greg-year-leap-year? a) "integer:year-number -> boolean"
    (and (= 0 (modulo a 4)) (or (not (= 0 (modulo a 100))) (= 0 (modulo a 400)))))

  (define-syntax-rule (greg-month-days-get leap-year?)
    (if leap-year? greg-month-days-leap-year greg-month-days))

  (define (greg-month->days a leap-year?)
    "integer boolean -> integer
    gives the number of days needed to reach the first day of the given month.
    months are from 1-12"
    (let ((month-days (greg-month-days-get leap-year?)) (end (- a 1)))
      (let loop ((index 0) (days 0))
        (if (< index end) (loop (+ 1 index) (+ days (vector-ref month-days index))) days))))

  (define (greg-year-days->month-and-day& a greg-month-days c)
    "get the month and month day after given days have passed from the beginning of the year"
    (let loop ((index 0) (days 0))
      (if (< index greg-number-of-months)
        (let (days (+ days (vector-ref greg-month-days index)))
          (if (< a days) (c (+ 1 index) (- (vector-ref greg-month-days index) (- days a 1)))
            (loop (+ 1 index) days)))
        (if (= a days) (c #f #f) (c (+ 1 index) (- (vector-ref greg-month-days index) (- days a)))))))

  (define (week-day-start-sunday->monday week-day) (if (= 0 week-day) 6 (- week-day 1)))

  (define (greg-week-day year month day)
    (week-day-start-sunday->monday
      (let* ((a (truncate-quotient (- 14 month) 12)) (y (- year a)) (m (- (+ month (* 12 a)) 2)))
        (modulo
          (+ day y
            (- (truncate-quotient y 4) (truncate-quotient y 100)) (truncate-quotient y 400)
            (truncate-quotient (* 31 m) 12))
          7))))

  (define (greg-year-first-week-day a) "integer:year-number -> week-day-number:0-6"
    (week-day-start-sunday->monday
      (let ((a (greg-year->years a)))
        (truncate-remainder
          (+ 1 (* 4 (truncate-remainder a 100))
            (* 5 (truncate-remainder a 4)) (* 6 (truncate-remainder a 400)))
          7))))

  (define (greg-year-weeks-53? a)
    "integer:year-number -> boolean
    check if the given year number corresponds to a year with 53 weeks according to the iso8601 standard"
    (let (week-day (greg-year-first-week-day a))
      (or (= 3 week-day) (and (= 2 week-day) (greg-year-leap-year? a))))))
