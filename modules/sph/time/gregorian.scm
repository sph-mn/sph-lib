(define-module (sph time gregorian))
(use-modules (sph))

(export greg-days->leap-days greg-days->year
  greg-days->year-days greg-days->years
  greg-month->days greg-month-days
  greg-month-days-get greg-month-days-leap-year
  greg-number-of-months greg-week-day
  greg-year-1970-days greg-year->years
  greg-year-days greg-year-days->month-and-day&
  greg-year-days-leap-year greg-year-first-week-day
  greg-year-leap-year? greg-year-weeks-53?
  greg-years->days greg-years->leap-days greg-years->year sph-time-gregorian-description)

(define sph-time-gregorian-description
  "gregorian calendar calculations
   uses a year 0 like iso8601. a year 0 appears to keep leap-day calculations simpler")

(define greg-month-days (vector 31 28 31 30 31 30 31 31 30 31 30 31))
(define greg-month-days-leap-year (vector 31 29 31 30 31 30 31 31 30 31 30 31))
(define greg-number-of-months 12)
(define greg-year-days 365)
(define greg-year-days-leap-year 366)
"days in years including leap years"
(define years-400-days 146096)
"days elapsed up to the beginning of 2-29"
(define month-2-29-days 59)
(define years-3-month-2-29-days 1154)
(define after-month-2-29-days (- greg-year-days-leap-year 31 28))
(define years-3-after-month-2-29-days (+ (* 3 greg-year-days) after-month-2-29-days))
(define years-4-days 1461)
(define years-100-days 36524)
(define greg-year-1970-days 719162)

(define (greg-years->year a)
  "does not work reliably when year is negative and part of a date with advanced months or days,
   because advancing days in a negative year reduce the number of elapsed years but not the year number"
  (+ a 1))

(define (greg-year->years a) (- a 1))

(define (greg-years->leap-days a)
  "integer -> integer
   the number of leap days that occured when given years have elapsed from the first day of the calendar.
   negative values for negative years
   year 0 is a leap year and begins after -1 years. the fifth negative year completes a leap year. the fourth (positive) year completes a new year"
  "without year 0, a leap year, the formula for positive numbers works the same. at the end we add the leap day from year 0"
  (if (negative? a)
    (let (a (- (abs a) 1))
      (+ 1 (- (truncate-quotient a 4) (- (truncate-quotient a 100) (truncate-quotient a 400)))))
    (- (truncate-quotient a 4) (- (truncate-quotient a 100) (truncate-quotient a 400)))))

(define (greg-years->days a)
  "integer -> integer
   gives the days contained in given number of fully elapsed years"
  ((if (negative? a) - +) (* a greg-year-days) (greg-years->leap-days a)))

(define (greg-days->year-days a leap-year?)
  "gives the number of days elapsed in a year. handles negative years/days"
  (if (negative? a) (+ (if leap-year? greg-year-days-leap-year greg-year-days) a) a))

(define (days-cycles& days c)
  "integer procedure:{cycles-400 rest-400 cycles-100 rest-100 cycles-4 rest-4 -> any} -> any
   gets the number of 400, 100 and 4 year cycles that occur in given days and their remainders.
   the full leap year cycle is 400 years long and contains 97 leap days.
   the algorithm counts different cycles from bigger to smaller while subtracting
   the days of the matched cycles before using the remaining days for the next step"
  (apply-values
    (l (cycles-400 rest-400)
      (apply-values
        (l (cycles-100 rest-100)
          (apply-values
            (l (cycles-4 rest-4) (c cycles-400 rest-400 cycles-100 rest-100 cycles-4 rest-4))
            (truncate/ rest-100 years-4-days)))
        (truncate/ rest-400 years-100-days)))
    (truncate/ days years-400-days)))

(define (greg-days->leap-days a)
  "integer -> integer
   gives the number of leap days in a given time span of full days.
   works with positive and negative day values and considers partial years where the leap day always falls on february 29.
   the calculation for negative values is similar to greg-years->leap-days, based on the fact that the formula for
   positive values can be used as long as year 0 is ignored.
   for day totals shorter than a year we check if the leap day in year 0 has passed.
   for longer day totals the contained cycles are counted, like for positive values."
  (if (negative? a)
    (let (a-abs (abs a))
      (if (<= a-abs greg-year-days-leap-year) (if (> a-abs after-month-2-29-days) 1 0)
        (+ 1
          (days-cycles& (- a-abs greg-year-days-leap-year)
            (l (cycles-400 rest-400 cycles-100 rest-100 cycles-4 rest-4)
              "check if the last day falls into centurial 4-year range, which does not include a leap day"
              (+ (* (abs cycles-400) 97) (* (abs cycles-100) 24)
                (abs cycles-4)
                (if (< (- years-100-days (abs rest-100)) years-4-days) 0
                  (if (>= (abs rest-4) years-3-after-month-2-29-days) 1 0))))))))
    (days-cycles& a
      (l (cycles-400 rest-400 cycles-100 rest-100 cycles-4 rest-4)
        "check if the last day falls into the centurial 4-year range, which does not include a leap day.
         (+ years-100-days rest-100) are the days after the last included 100 year cycle"
        (+ (* (abs cycles-400) 97) (* (abs cycles-100) 24)
          (abs cycles-4)
          (if (< (- years-100-days rest-100) years-4-days) 0
            (if (< rest-4 years-3-month-2-29-days) 0 1)))))))

(define (greg-days->years a)
  "integer -> integer
   the number of years the given number of days fill"
  (truncate-quotient ((if (negative? a) + -) a (greg-days->leap-days a)) greg-year-days))

(define (greg-days->year a)
  (let (years (/ ((if (negative? a) + -) a (greg-days->leap-days a)) greg-year-days))
    "floor is the largest integer less than or equal to x"
    (if (and (negative? a) (zero? years)) 0 (greg-years->year (floor years)))))

(define (greg-year-leap-year? a)
  "integer:year-number -> boolean
   check if the given year is a leap year"
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
  "integer #(days-of-month ...) procedure:{month day -> any} -> any
   get the month and month day after the given number of days have passed starting from the beginning of the year"
  (let loop ((index 0) (days 0))
    (if (< index greg-number-of-months)
      (let (days (+ days (vector-ref greg-month-days index)))
        (if (< a days) (c (+ 1 index) (- (vector-ref greg-month-days index) (- days a 1)))
          (loop (+ 1 index) days)))
      (if (= a days) (c #f #f) (c (+ 1 index) (- (vector-ref greg-month-days index) (- days a)))))))

(define (week-day-start-sunday->monday week-day) (if (= 0 week-day) 6 (- week-day 1)))

(define (greg-week-day year month day)
  "integer integer integer -> integer
   0 being monday"
  (week-day-start-sunday->monday
    (let* ((a (truncate-quotient (- 14 month) 12)) (y (- year a)) (m (- (+ month (* 12 a)) 2)))
      (modulo
        (+ day y
          (- (truncate-quotient y 4) (truncate-quotient y 100)) (truncate-quotient y 400)
          (truncate-quotient (* 31 m) 12))
        7))))

(define (greg-year-first-week-day a) "integer:year-number -> integer:0-6:week-day-number"
  "could be solved with greg-week-day but this seems more efficient"
  (week-day-start-sunday->monday
    (let ((a (greg-year->years a)))
      (truncate-remainder
        (+ 1 (* 4 (truncate-remainder a 100))
          (* 5 (truncate-remainder a 4)) (* 6 (truncate-remainder a 400)))
        7))))

(define (greg-year-weeks-53? a)
  "integer:year-number -> boolean
   check if the given year number corresponds to a year with 53 instead of 52 weeks according to the iso8601 standard"
  (let (week-day (greg-year-first-week-day a))
    (or (= 3 week-day) (and (= 2 week-day) (greg-year-leap-year? a)))))
