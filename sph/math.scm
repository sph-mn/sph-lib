(library (sph math)
  (export
    absolute-difference
    average
    bit->byte-length
    decrement-one
    golden-ratio
    increment-one
    integer-and-fraction&
    log-base
    number-length
    percent
    percent-as-integer
    round-to-decimal-places
    round-to-increment
    signed-integer-binary-length
    simple-format-number
    truncate-to-decimal-places
    unsigned-integer-binary-length)
  (import
    (ice-9 format)
    (rnrs base)
    (sph)
    (only (guile)
      simple-format
      inexact->exact
      exact->inexact))

  (define golden-ratio 1.6180339887)

  (define (round-to-increment a increment)
    "number number -> number
    round a number to the the nearest multiple of \"increment\" (using \"round\").
    if the number is exactly half-way in between increments, take the lower increment.
    example: (1.1 3) -> 0, (1.6 3) -> 3, (1.5 3) -> 0"
    (* (round (/ a increment)) increment))

  (define (bit->byte-length a) (inexact->exact (ceiling (/ a 8))))
  (define (percent value base) "how many percent is value from base" (/ (* value 100) base))
  (define-syntax-rule (percent-as-integer value base) (round (inexact->exact (percent value base))))

  (define (integer-and-fraction& a c)
    "number procedure:{integer real} -> any
    splits a number into its integer and fractional part. example: 1.74 -> 1 0.74"
    (let (integer (truncate a)) (c integer (- a integer))))

  (define (signed-integer-binary-length a)
    "integer -> integer
    calculates the minimum number of digits required to represent \"a\" in base 2"
    (let (a (abs a)) (if (< a 2) 2 (+ 1 (inexact->exact (ceiling (/ (log (+ 1 a)) (log 2))))))))

  (define (unsigned-integer-binary-length a)
    "integer -> integer
    calculates the minimum number of digits required to represent \"a\" in base 2"
    (let (a (abs a)) (if (< a 2) 2 (+ 1 (inexact->exact (ceiling (/ (log (+ 1 a)) (log 2))))))))

  (define (log-base a base)
    "number number -> number
    result in the logarithm with \"base\" of \"a\""
    (/ (log a) (log base)))

  (define (number-length a base)
    "number number -> number
    calculate the number of digits of \"a\" represented in base \"base\""
    (+ 1 (floor (log-base a base))))

  (define (round-to-decimal-places a decimal-places) "number number -> number"
    (let (modifier (expt 10 decimal-places)) (/ (round (* a modifier)) modifier)))

  (define (truncate-to-decimal-places a decimal-places) "number number -> number"
    (let (modifier (expt 10 decimal-places)) (/ (truncate (* a modifier)) modifier)))

  (define (increment-one a) (+ 1 a))
  (define (decrement-one a) (- a 1))

  (define (average . a)
    "number ... -> number
    calculate the average of the given numbers"
    (/ (apply + a) (length a)))

  (define (absolute-difference n-1 n-2)
    "number number -> number
    result in the non-negative difference of two numbers"
    (abs (- n-1 n-2)))

  (define* (simple-format-number a #:optional (decimal-point-shift 0) (decimal-places 0))
    "number integer integer -> string
    number as a string, rounded to decimal places, decimal mark shifted by n steps left (the direction where the string representation of an increasing number would grow towards)"
    (if (= 0 decimal-places) (number->string (round (/ a (expt 10 decimal-point-shift))))
      (format #f (simple-format #f "~~,~A,~Af" decimal-places (* -1 decimal-point-shift)) a))))
