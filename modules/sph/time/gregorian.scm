(library (sph time gregorian)
  (export
    greg-days->leap-days
    greg-days->years
    greg-month->days
    greg-month-days
    greg-month-days-get
    greg-month-days-leap-year
    greg-number-of-months
    greg-year-days
    greg-year-days->month-and-day&
    greg-year-leap-year?
    greg-years->days
    greg-years->leap-days)
  (import
    (rnrs base)
    (sph)
    (only (guile)
      quotient
      modulo
      truncate/)
    (only (sph one) apply-values))

  (define-as greg-month-days vector 31 28 31 30 31 30 31 31 30 31 30 31)
  (define-as greg-month-days-leap-year vector 31 29 31 30 31 30 31 31 30 31 30 31)
  (define greg-number-of-months 12)
  (define years-400-days 146097)
  (define years-4-days-no-leap 1460)
  (define years-4-days 1461)
  (define years-100-days 36524)
  (define greg-year-days 365)

  (define (greg-years->leap-days a) "number of leap days before the year was reached"
    (if (< a 4) 0 (- (quotient a 4) (- (quotient a 100) (quotient a 400)))))

  (define (greg-years->days a) "elapsed days to reach given year"
    (+ (* a greg-year-days) (greg-years->leap-days a)))

  (define (greg-days->leap-days a)
    "gives the number of leap days in a given time span of days since year 1"
    (apply-values
      (l (cycles-400 rest)
        (+ (* cycles-400 97) (- (quotient rest years-4-days) (quotient rest years-100-days))))
      (truncate/ a years-400-days)))

  (define (greg-days->years a) (quotient (- a (greg-days->leap-days a)) greg-year-days))

  (define (greg-year-leap-year? a)
    (and (= 0 (modulo a 4)) (or (not (= 0 (modulo a 100))) (= 0 (modulo a 400)))))

  (define-syntax-rule (greg-month-days-get leap-year?)
    (if leap-year? greg-month-days-leap-year greg-month-days))

  (define (greg-month->days a leap-year?) "months are from 1-12"
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
        (c #f #f)))))
