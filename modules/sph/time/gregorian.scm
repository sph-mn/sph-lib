(library (sph time gregorian)
  (export
    greg-days->leap-days
    greg-days->year
    greg-month->days
    greg-month-days
    greg-month-days-get
    greg-month-days-leap-year
    greg-number-of-months
    greg-year->days
    greg-year->leap-days
    greg-year-days
    greg-year-days->month-and-day&
    greg-year-leap-year?)
  (import
    (rnrs base)
    (sph)
    (only (guile) quotient modulo))

  (define-as greg-month-days vector 31 28 31 30 31 30 31 31 30 31 30 31)
  (define-as greg-month-days-leap-year vector 31 29 31 30 31 30 31 31 30 31 30 31)
  (define greg-number-of-months 12)
  (define years-400-days 146097)
  (define years-4-days-no-leap 1460)
  (define years-100-days 36524)
  (define greg-year-days 365)
  (define (greg-year->leap-days a) (- (quotient a 4) (- (quotient a 100) (quotient a 400))))

  (define (greg-days->leap-days a)
    (- (quotient a years-4-days-no-leap)
      (- (quotient a years-100-days) (quotient a years-400-days))))

  (define (greg-days->year a) (truncate (/ (- a (greg-days->leap-days a)) greg-year-days)))
  (define (greg-year->days a) (+ (* a greg-year-days) (greg-year->leap-days a)))

  (define (greg-year-leap-year? a)
    (and (= 0 (modulo a 4)) (or (not (= 0 (modulo a 100))) (= 0 (modulo a 400)))))

  (define-syntax-rule (greg-month-days-get leap-year?)
    (if leap-year? greg-month-days-leap-year greg-month-days))

  (define (greg-month->days a leap-year?)
    (let ((month-days (greg-month-days-get leap-year?)) (end (if (= 0 a) a (- a 1))))
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
