(library (sph number)
  (export
    absolute-difference
    average
    bit->byte-length
    call-with-multiple/
    container-length->number-max
    decrement-one
    golden-ratio
    increment-one
    integer-and-fraction&
    log-base
    number-container-length
    percent
    round-to-decimal-places
    round-to-increment
    simple-format-number
    truncate-to-decimal-places)
  (import
    (ice-9 format)
    (rnrs arithmetic flonums)
    (rnrs base)
    (sph)
    (only (guile)
      simple-format
      string-split
      inexact->exact
      exact->inexact))

  (define golden-ratio 1.6180339887)

  (define (round-to-increment a increment)
    "number number -> number
    round a number to the the nearest multiple of \"increment\" (using \"round\").
    if the number is exactly half-way in between increments, take the lower increment multiple.
    example: (1.1 3) -> 0, (1.6 3) -> 3, (1.5 3) -> 0"
    (* (round (/ a increment)) increment))

  (define (bit->byte-length a)
    "integer -> integer
    calculate the bytes required to store the number of bits"
    (inexact->exact (ceiling (/ a 8))))

  (define (container-length->number-max digit-count base)
    "integer integer -> integer
    calculate the maximum value that can represented with the given number of digits in the given base"
    (- (expt base digit-count) 1))

  (define (number-container-length a base)
    "integer:positive-integer integer -> integer
    results in the number of vector elements of size base required to store the individual digits of the given positive number in the given base.
    example use case is calculating the size of a bytevector for storing an integer"
    (if (= 0 a) 0 (+ 1 (inexact->exact (floor (log-base a base))))))

  (define (percent value base) "how many percent is value from base" (/ (* value 100) base))

  (define (integer-and-fraction& a c)
    "number procedure:{integer real -> any:result} -> any:result
    splits a number into its integer and fractional part. example: 1.74 -> 1 0.74"
    ;this implementation is slow, but it works with real numbers without rounding errors
    (apply
      (l (integer fraction)
        (c (string->number integer) (string->number (string-append "0." fraction))))
      (string-split (number->string a) #\.)))

  (define (log-base a base)
    "number number -> number
    result in the logarithm with \"base\" of \"a\""
    (/ (log a) (log base)))

  (define (call-with-multiple/ proc a factor)
    "procedure:{number -> number} number number -> number
    call proc with \"a\" multiplied by factor and afterwards divide by factor"
    (/ (proc (* a factor)) factor))

  (define (round-to-decimal-places a decimal-places) "number number -> number"
    (call-with-multiple/ round a (expt 10 decimal-places)))

  (define (truncate-to-decimal-places a decimal-places) "number number -> number"
    (call-with-multiple/ truncate a (expt 10 decimal-places)))

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
      (let
        (pattern-format (simple-format #f "~~,~A,~Af" decimal-places (* -1 decimal-point-shift)))
        (format #f pattern-format a)))))
