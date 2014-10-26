(library (sph random-data)
  (export
    primitive-random
    random
    random-ascii-string
    random-bytevector
    random-list
    random-string)
  (import
    (rnrs base)
    (rnrs bytevectors)
    (sph)
    (only (guile) random-state-from-platform)
    (only (sph char-set-vector) char-set-vector:designated)
    (only (sph list) n-times-map)
    (rename (guile) (random primitive-random)))

  (define random-state (random-state-from-platform))

  (define* (random max-value #:optional (min-value 0) (state random-state))
    "integer [integer random-state] -> integer
    create an integer between min-value and max-value inclusively"
    (+ min-value (primitive-random (- max-value min-value) state)))

  (define* (random-list list-length #:optional (max-value 255) (min-value 0) (state random-state))
    "integer integer integer random-state -> (integer ...)
    create a list of integers with random-integer"
    (n-times-map list-length (l (n) (random max-value min-value state))))

  (define*
    (random-chars list-length #:optional (char-set char-set-vector:designated) (state random-state))
    "integer string/vector random-state -> (character ...)
    creates a list of random chars from a set of characters.
    the default set of characters includes all the code points to which unicode has assigned a character or other meaning"
    (let*
      ( (char-set (if (string? char-set) (string->utf8 char-set) char-set))
        (char-set-last-index (- (vector-length char-set) 1)))
      (n-times-map list-length (l (n) (vector-ref char-set (random char-set-last-index 0 state))))))

  (define (random-bytevector size) "integer -> bytevector"
    (let (r (make-bytevector size))
      (let loop ((index 0))
        (if (< index size) (begin (bytevector-u8-set! r index (random 256)) (loop (+ 1 index))) r))))

  (define*
    (random-string #:optional (len (random 255)) (char-set char-set-vector:designated)
      (state random-state))
    "[integer string/vector] -> string
    the default set of characters includes all the code points to which unicode has assigned a character or other meaning"
    (list->string (random-chars len char-set state)))

  (define (random-ascii-string len)
    "integer -> string
    results in a string of randomly chosen ascii characters excluding control characters"
    (let loop ((n 0) (r (list)))
      (if (< n len) (loop (+ 1 n) (cons (integer->char (+ 32 (random 94))) r)) (list->string r)))))