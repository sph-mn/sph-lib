(library (sph random-data)
  (export
    random-ascii-string
    random-bytevector
    random-integer
    random-integer-list
    random-length-string
    random-string)
  (import
    (rnrs base)
    (sph)
    (only (guile)
      call-with-input-file
      random
      random-state-from-platform)
    (only (rnrs bytevectors) string->utf8)
    (only (rnrs io ports) get-bytevector-n)
    (only (sph char-set-vector) char-set-vector:designated)
    (only (sph list) n-times-map))

  (define random-state (random-state-from-platform))

  (define* (random-integer max-value #:optional (min-value 0) (state random-state))
    "integer [integer random-state] -> integer
    create an integer between min-value and max-value inclusively"
    (+ min-value (random (+ (- max-value min-value) 1) state)))

  (define*
    (random-integer-list list-length #:optional (max-value 255) (min-value 0) (state random-state))
    "integer integer integer random-state -> (integer ...)
    create a list of integers with random-integer"
    (n-times-map list-length (l (n) (random-integer max-value min-value state))))

  (define*
    (random-chars list-length #:optional (char-set char-set-vector:designated) (state random-state))
    "integer string/vector random-state -> (character ...)
    creates a list of random chars from a set of characters.
    the default set of characters includes all the code points to which unicode has assigned a character or other meaning"
    (let*
      ( (char-set (if (string? char-set) (string->utf8 char-set) char-set))
        (char-set-last-index (- (vector-length char-set) 1)))
      (n-times-map list-length
        (l (n) (vector-ref char-set (random-integer char-set-last-index 0 state))))))

  (define (random-bytevector size)
    "integer -> bytevector
    reads from /dev/urandom to create a bytevector"
    (call-with-input-file "/dev/urandom" (l (port) (get-bytevector-n port size))))

  (define*
    (random-string #:optional (len (random-integer 255)) (char-set char-set-vector:designated)
      (state random-state))
    "[integer string/vector] -> string
    the default set of characters includes all the code points to which unicode has assigned a character or other meaning"
    (list->string (random-chars len char-set state)))

  (define (random-ascii-string len)
    "integer -> string
    results in a string of randomly chosen ascii characters excluding control characters"
    (let next ((n 0) (res (list)))
      (if (< n len) (next (+ 1 n) (cons (integer->char (+ 32 (random-integer 94))) res))
        (list->string res)))))