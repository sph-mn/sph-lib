(library (sph vector selection)
  (export
    selection-complexity-maximum
    selection-distinct-maximum
    vector-complexity
    vector-numeric-increment-be
    vector-numeric-increment-be!
    vector-numeric-increment-le
    vector-numeric-increment-le!
    vector-selection
    vector-selection-stream
    vector-selections)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph error)
    (srfi srfi-41)
    (only (guile) vector-copy))

  ;vectors as selections from a set

  (define (vector-selection set-indices set)
    "vector:#(integer ...) vector -> vector
    set-indices is a list of indexes, set is a selection. reorders selection by indexes in set-indices"
    (vector-map (l (index) (vector-ref set index)) set-indices))

  (define (vector-numeric-increment-le! a base)
    "vector integer -> true/false
    like vector-numeric-increment-le but modifies the vector"
    (let (a-length (vector-length a))
      (let loop ((index 0))
        (if (>= index a-length) #f
          (if (< (vector-ref a index) base)
            (begin (vector-set! a index (+ (vector-ref a index) 1)) #t)
            (begin (vector-set! a index 0) (loop (+ index 1))))))))

  (define (vector-numeric-increment-be! a base)
    "vector integer -> true/false
    like \"vector-numeric-increment-be\" but modifies the vector"
    (let loop ((index (- (vector-length a) 1)))
      (if (>= index 0)
        (if (< (vector-ref a index) base)
          (begin (vector-set! a index (+ (vector-ref a index) 1)) #t)
          (begin (vector-set! a index 0) (loop (- index 1))))
        #f)))

  (define (vector-numeric-increment-le a base)
    "vector integer -> vector
    treat a vector of numbers as a whole like a number to \"base\" in little endian and increment it.
    returns false if the maximum value has been reached"
    (let (r (vector-copy a)) (and (vector-numeric-increment-le! r base) r)))

  (define (vector-numeric-increment-be a base)
    "vector integer -> vector
    treat a vector of numbers as a whole like a number to \"base\" in big endian and increment it
    returns false if the maximum value has been reached"
    (let (r (vector-copy a)) (and (vector-numeric-increment-be! r base) r)))

  (define* (vector-selections set #:optional width)
    "vector integer -> (vector ...)
    return a list of selections for vector \"set\" with duplicate elements allowed. set can contain any datatype.
    the optional parameter width specifies the length of each resulting selection.
    for example, width: 2 creates all two-element selections of set.
    the default for width is the length of the set"
    (let*
      ( (last-index (- (vector-length set) 1))
        (set-indices-init (make-vector (+ (if width (- width 1) last-index) 1) 0)))
      (let loop ((set-indices set-indices-init) (r (list (vector-selection set-indices-init set))))
        (let (set-indices (vector-numeric-increment-le set-indices last-index))
          (if set-indices (loop set-indices (pair (vector-selection set-indices set) r)) r)))))

  (define* (vector-selection-stream selection #:optional (width (vector-length selection)))
    "vector width -> stream
    creates an srfi-41-stream of selections with duplicates of selection with a specific width"
    (let ((v (make-vector width 0)) (selection-last-index (- (vector-length selection) 1)))
      (stream-let next ((v (vector-numeric-increment-le v selection-last-index)))
        (if v
          (stream-cons (vector-selection v selection)
            (next (vector-numeric-increment-le v selection-last-index)))
          stream-null))))

  (define (selection-count-one-self start-p-index width selection start-index)
    "integer integer vector integer -> integer
    count one or zero occurences of a selection in vector selection specified by width and start-index"
    (let ((selection-length (vector-length selection)) (end-index (+ start-p-index (- width 1))))
      (let loop ((c-index start-index) (p-index start-p-index))
        (if (< c-index selection-length)
          (if (equal? (vector-ref selection p-index) (vector-ref selection c-index))
            (if (= p-index end-index) 1 (loop (+ 1 c-index) (+ 1 p-index)))
            (loop (if (eqv? p-index start-p-index) (+ 1 c-index) c-index) start-p-index))
          0))))

  (define* (selection-distinct-maximum set-length #:optional (selection-width set-length))
    "integer integer -> integer
    calculate the maximum number of distinct selections of a set with length set-length and optional width which defaults to set-length"
    (if (= 0 set-length) 0 (expt set-length selection-width)))

  (define* (selection-complexity-maximum width #:optional (min-width 1))
    "integer integer -> integer
    calculate the maximum number of possible unique sub-selections in a selection up to width, optionally ignoring widths smaller than min-width"
    (if (= width 0) width
      (if (>= min-width width) 1
        (+ (- (+ width 1) min-width) (selection-complexity-maximum width (+ min-width 1))))))

  (define* (vector-complexity selection #:optional (min-pw 1) max-pw)
    "vector integer integer -> integer
    find the number of distinct sub-selections in a selection.
    a sub-selection is counted if its values, order or length are distinct"
    ;pw: selection-width
    (let*
      ((selection-length (vector-length selection)) (max-pw (if max-pw max-pw selection-length)))
      (if (> min-pw max-pw) #f
        (let next-level ((level max-pw) (occurence 0))
          (if (<= min-pw level)
            (next-level (- level 1)
              (+
                (let ((max-length (- selection-length (- level 1))))
                  (let next-selection ((index 0) (count 0))
                    (if (< index max-length)
                      (next-selection (+ 1 index)
                        (+ (selection-count-one-self index level selection (+ index 1)) count))
                      count)))
                occurence))
            (- (selection-complexity-maximum max-pw min-pw) occurence)))))))
