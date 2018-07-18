(library (sph number)
  (export
    absolute-difference
    average
    bit->byte-length
    bound
    bound-max
    bound-min
    call-with-product-then-divide
    container-length->number-max
    decrement-one
    float-sum
    fraction
    in-between?
    in-range?
    increment-one
    integer-and-fraction
    log-base
    number-container-length
    number-format-float
    percent
    round-to-decimal-places
    round-to-increment
    truncate-to-decimal-places)
  (import
    (ice-9 format)
    (rnrs arithmetic flonums)
    (sph)
    (only (guile)
      string-split
      string-rindex
      floor
      inexact->exact
      exact->inexact)
    (only (sph string) string-multiply))

  (define (float-sum . a)
    "return the sum of the given numbers calculated with rounding error compensation.
     uses kahan summation with neumaier modification"
    (let loop ((rest (tail a)) (result (first a)) (correction 0.0))
      (if (null? rest) (+ correction result)
        (let* ((a (first rest)) (b (+ a result)))
          (loop (tail rest) b
            ; the summation with "correction" must be a separate call, did not work otherwise
            (+ correction (if (>= result a) (+ (- result b) a) (+ (- a b) result))))))))

  (define (in-between? n start end)
    "number number number -> boolean
     true if n is between and not equal to num-start or num-end"
    (and (> n start) (< n end)))

  (define (in-range? n start end)
    "number number number -> boolean
     true if n is between or equal to start or end"
    (and (>= n start) (<= n end)))

  (define (bound n min max)
    "number number -> number
     if n is smaller than min, return min.
     if n is greater than max, return max"
    (if (> n max) max (if (< n min) min n)))

  (define-syntax-rule (bound-min a min) (max a min))
  (define-syntax-rule (bound-max a max) (min a max))

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

  (define (percent value base) "how many percent is value from base"
    (if (zero? base) 0 (/ (* value 100) base)))

  (define (integer-and-fraction a c)
    "number procedure:{integer real -> any:result} -> any:result
     splits a number into its integer and fractional part. example: 1.74 -> 1 0.74"
    ; algorithm: convert to string, remove integer part, convert to number.
    ; slow, but it works for real numbers without rounding errors
    (apply
      (l (integer fraction)
        (c (string->number integer) (string->number (string-append "0." fraction))))
      (string-split (number->string a) #\.)))

  (define (fraction a)
    (let*
      ( (a (string-split (number->string (exact->inexact a)) #\.))
        (fraction-string (if (= 2 (length a)) (second a) "0")))
      (string->number (string-append "0." fraction-string))))

  (define (log-base a base)
    "number number -> number
     result in the logarithm with \"base\" of \"a\""
    (/ (log a) (log base)))

  (define (call-with-product-then-divide f a factor)
    "procedure:{number -> number} number number -> number
     call f with \"a\" multiplied by factor and afterwards divide by factor"
    (/ (f (* a factor)) factor))

  (define (round-to-decimal-places a decimal-places) "number number -> number"
    (call-with-product-then-divide round a (expt 10 decimal-places)))

  (define (truncate-to-decimal-places a decimal-places) "number number -> number"
    (call-with-product-then-divide truncate a (expt 10 decimal-places)))

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

  (define* (number-format-float a #:key decimal-min decimal-max truncate* (base 10)) "number"
    (let*
      ( (a (call-with-product-then-divide (or truncate* truncate) a (expt base (or decimal-max 0))))
        (a (number->string (if (integer? a) (inexact->exact a) (exact->inexact a)) base))
        (decimal-min (or decimal-min 0))
        (decimal-length (let (b (string-rindex a #\.)) (if b (- (string-length a) b 1) 0))))
      (if (< decimal-length decimal-min)
        (string-append a (if (zero? decimal-length) "." "")
          (string-multiply "0" (- decimal-min decimal-length)))
        a))))
