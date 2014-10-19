(library (sph math)
  (export
    abs-difference
    average
    bit->byte-length
    decrement-one
    golden-ratio
    increment-one
    number-length
    percent
    percent-as-integer
    round-to-decimal-places
    round-to-increment
    signed-integer-binary-length
    simple-format-number->string
    truncate-to-decimal-places
    unsigned-integer-binary-length)
  (import
    (rnrs base)
    (sph)
    (only (guile) inexact->exact exact->inexact))

  (define golden-ratio 1.6180339887)
  (define (round-to-increment arg increment) (* (round (/ arg increment)) increment))

  ;currently math and number procedures mixed

  (define (bit->byte-length arg) (inexact->exact (ceiling (/ arg 8))))
  (define (percent value base) "how many percent is value from base" (/ (* value 100) base))
  (define-syntax-rule (percent-as-integer value base) (round (inexact->exact (percent value base))))

  (define (signed-integer-binary-length arg)
    (let (arg (abs arg))
      (if (< arg 2) 2 (+ 1 (inexact->exact (ceiling (/ (log (+ 1 arg)) (log 2))))))))

  (define (unsigned-integer-binary-length arg)
    (let (arg (abs arg))
      (if (< arg 2) 2 (+ 1 (inexact->exact (ceiling (/ (log (+ 1 arg)) (log 2))))))))

  (define (number-length arg base) (string-length (number->string arg base)))

  (define (round-to-decimal-places arg decimal-places)
    (let (modifier (expt 10 decimal-places)) (/ (round (* arg modifier)) modifier)))

  (define (truncate-to-decimal-places arg decimal-places)
    (let (modifier (expt 10 decimal-places)) (/ (truncate (* arg modifier)) modifier)))

  (define (increment-one arg) (+ 1 arg))
  (define (decrement-one arg) (- arg 1))

  (define (average . args)
    "number ... -> number
    calculate the average of the given numbers"
    (/ (apply + args) (length args)))

  (define (abs-difference n-1 n-2)
    "number number -> number
    result in the non-negative difference of two numbers"
    (abs (- n-1 n-2)))

  (define* (simple-format-number->string arg #:optional (decimal-point-shift 0) (decimal-places 0))
    "number integer integer-> string
    number as a string, rounded to decimal places"
    (number->string (round-to-decimal-places (/ arg (expt 10 decimal-point-shift)) decimal-places))))