(library (sph vector selection)
  (export
    sph-vector-selection-description
    tuple-complexity-maximum
    tuple-distinct-maximum
    vector-complexity
    vector-numeric-increment-be
    vector-numeric-increment-be!
    vector-numeric-increment-le
    vector-numeric-increment-le!
    vector-selection
    vector-selections
    vector-selections-stream)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph error)
    (srfi srfi-41)
    (only (guile) vector-copy))

  (define sph-vector-selection-description
    "create and analyse selections from sets: permutations, combinations, n-tuples")

  (define (vector-numeric-increment-le! a base)
    "vector integer -> true/false
    like \"vector-numeric-increment-le\" but modifies the input vector"
    (let (a-length (vector-length a))
      (let loop ((index 0))
        (if (>= index a-length) #f
          (if (< (vector-ref a index) base)
            (begin (vector-set! a index (+ (vector-ref a index) 1)) #t)
            (begin (vector-set! a index 0) (loop (+ index 1))))))))

  (define (vector-numeric-increment-be! a base)
    "vector integer -> true/false
    like \"vector-numeric-increment-be\" but modifies the input vector"
    (let loop ((index (- (vector-length a) 1)))
      (if (>= index 0)
        (if (< (vector-ref a index) base)
          (begin (vector-set! a index (+ (vector-ref a index) 1)) #t)
          (begin (vector-set! a index 0) (loop (- index 1))))
        #f)))

  (define (vector-numeric-increment-le a base)
    "vector integer -> vector
    treat a vector of integers as a whole like one number to \"base\" in little endian and increment it.
    returns false if the maximum value has been reached"
    (let (r (vector-copy a)) (and (vector-numeric-increment-le! r base) r)))

  (define (vector-numeric-increment-be a base)
    "vector integer -> vector
    treat a vector of integers as a whole like one number to \"base\" in big endian and increment it
    returns false if the maximum value has been reached"
    (let (r (vector-copy a)) (and (vector-numeric-increment-be! r base) r)))

  (define (vector-selection set-indices set)
    "vector:#(integer ...) vector -> vector
    return a new vector of values selected at indices in set"
    (vector-map (l (index) (vector-ref set index)) set-indices))

  (define* (vector-selections set #:optional width)
    "vector integer -> (vector ...)
    return a list of all possible arrangements of values from \"set\" with duplicate elements allowed. set can contain any datatype.
    the optional parameter \"width\" specifies the length of selections.
    for example, width: 2 creates all possible two element arrangements of set.
    the default for \"width\" is the length of the set"
    (let*
      ( (last-index (- (vector-length set) 1))
        (set-indices-init (make-vector (+ (if width (- width 1) last-index) 1) 0)))
      (let loop ((set-indices set-indices-init) (r (list (vector-selection set-indices-init set))))
        (let (set-indices (vector-numeric-increment-le set-indices last-index))
          (if set-indices (loop set-indices (pair (vector-selection set-indices set) r)) r)))))

  (define* (vector-selections-stream selection #:optional (width (vector-length selection)))
    "vector width -> stream
    like vector-selections but returns an srfi-41-stream and calculates next results on demand"
    (let ((v (make-vector width 0)) (selection-last-index (- (vector-length selection) 1)))
      (stream-let next ((v (vector-numeric-increment-le v selection-last-index)))
        (if v
          (stream-cons (vector-selection v selection)
            (next (vector-numeric-increment-le v selection-last-index)))
          stream-null))))

  (define* (tuple-distinct-maximum set-length #:optional (selection-width set-length))
    "integer integer -> integer
    calculate the maximum number of possible arrangements for a set with length \"set-length\" and optional \"width\" which defaults to \"set-length\""
    (if (= 0 set-length) 0 (expt set-length selection-width)))

  (define* (tuple-complexity-maximum width #:optional (min-width 1))
    "integer integer -> integer
    calculate the maximum number of possible distinct tuples in a tuple up to width, optionally ignoring widths smaller than min-width"
    (if (or (= 0 width) (< width min-width)) 0
      (if (= min-width width) 1
        (+ (- (+ 1 width) min-width) (tuple-complexity-maximum width (+ 1 min-width))))))

  (define (vector-complexity-count-one start-index width tuple tuple-length)
    "integer integer vector integer -> integer
    helper for \"vector-complexity\""
    ; ???
    (let (end-index (+ start-index (- width 1)))
      (let loop ((index (+ 1 start-index)) (match-index start-index))
        (debug-log (q index) index (q match-index) match-index)
        (if (< index tuple-length)
          (begin (debug-log (q equal?) (vector-ref tuple match-index) (vector-ref tuple index))
            (if (equal? (vector-ref tuple match-index) (vector-ref tuple index))
              (if (= match-index end-index) 1 (loop (+ 1 index) (+ 1 match-index)))
              (loop (if (eqv? match-index start-index) (+ 1 index) index) start-index)))
          0))))

  (define* (vector-complexity tuple #:optional (min-width 1) max-width)
    "vector integer integer -> integer
    count all distinct tuples in a tuple.
    distinctness is defined by elements, order and length"
    ; how sub-tuples are counted:
    ; #([1 2 3] 4)
    ; #(1 [2 3 4])
    ; #([1 2] 3 4)
    ; #(1 [2 3] 4)
    ; #(1 2 [3 4])
    (let* ((tuple-length (vector-length tuple)) (max-width (if max-width max-width tuple-length)))
      (if (> min-width max-width) #f
        (let next-level ((level max-width) (duplicates 0))
          (debug-log (q level) level)
          (if (<= min-width level)
            (next-level (- level 1)
              (+
                (let ((last-index (- tuple-length level)))
                  (let next-tuple ((index 0) (count 0))
                    (if (<= index last-index)
                      (next-tuple (+ index 1)
                        (+ count (vector-complexity-count-one index level tuple tuple-length)))
                      count)))
                duplicates))
            (- (tuple-complexity-maximum max-width min-width) duplicates)))))))
