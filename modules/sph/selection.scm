(define-module (sph selection))

(use-modules (sph) (sph hashtable)
  (srfi srfi-41) (srfi srfi-1)
  ((sph list) #:select (map-integers map-map produce))

  ((sph vector) #:select (vector-range ))
  ((rnrs base) #:select (vector-map)))

(export divisions number-divisions
  sph-selection-description vector-distinct-count
  vector-distinct-maximum vector-distinct-stream
  vector-numeric-increment-be vector-numeric-increment-be!
  vector-numeric-increment-le vector-numeric-increment-le!
  vector-selection vector-selection-maximum vector-selections vector-selections-stream)

(define sph-selection-description
  "create and analyse set selections: permutations, combinations and similar")

(define-syntax-rule (vector-distinct-set-create tuple-length width)
  (set-create-empty (+ 1 (- tuple-length width))))

(define (set-create-empty initial-size) (ht-make ht-hash-equal equal? initial-size))
(define (set-contains? a value) (ht-contains? a value))
(define (set-add! a value) (ht-set! a value #t))

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
      (if (< (vector-ref a index) base) (begin (vector-set! a index (+ (vector-ref a index) 1)) #t)
        (begin (vector-set! a index 0) (loop (- index 1))))
      #f)))

(define (vector-numeric-increment-le a base)
  "vector integer -> vector
   treat integers in a vector as digits of a number to \"base\" and increment it.
   the least significant digit is the first element of the vector.
   return false if the maximum value has been reached"
  (let (r (vector-copy a)) (and (vector-numeric-increment-le! r base) r)))

(define (vector-numeric-increment-be a base)
  "vector integer -> vector
   treat integers in a vector as digits of a number to \"base\" and increment it.
   the least significant digit is the last element of the vector.
   returns false if the maximum value has been reached"
  (let (r (vector-copy a)) (and (vector-numeric-increment-be! r base) r)))

(define (vector-selection set-indices set)
  "vector:#(integer ...) vector -> vector
   return a new vector of values at indices in set"
  (vector-map (l (index) (vector-ref set index)) set-indices))

(define* (vector-selections set #:optional width)
  "vector integer -> (vector ...)
   return a list of all distinct selections of values from \"set\" with duplicate elements allowed. set can contain any datatype.
   the optional parameter \"width\" specifies the length of selections.
   for example, a width of two creates all possible two element selections of set.
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

(define* (vector-selection-maximum set-length #:optional (selection-width set-length))
  "integer integer -> integer
   calculate the maximum number of possible distinct selections from a set with length \"set-length\" and
   optional \"selection-width\" which defaults to \"set-length\""
  (if (= 0 set-length) 0 (expt set-length selection-width)))

(define* (vector-distinct-maximum width #:optional (min-width 1))
  "integer integer -> integer
   calculate the maximum number of possible distinct tuples in a tuple up to width, optionally ignoring widths smaller than min-width"
  (if (or (= 0 width) (< width min-width)) 0
    (if (= min-width width) 1
      (+ (- (+ 1 width) min-width) (vector-distinct-maximum width (+ 1 min-width))))))

(define* (vector-distinct-count a #:optional (min-width 1) max-width)
  "vector integer integer -> integer
   count all distinct sub-vectors in a vector with lengths from min-width to max-width.
   distinctness is defined by length, order and value.
   how sub-vectors are counted:
   #([1 2 3] 4)
   #(1 [2 3 4])
   #([1 2] 3 4)
   #(1 [2 3] 4)
   #(1 2 [3 4])"
  (let* ((a-length (vector-length a)) (width (or max-width a-length)))
    (if (> min-width width) 0
      (if (= min-width width) 1
        (let loop
          ( (known (vector-distinct-set-create a-length width)) (width width) (index 0)
            (last-index (- a-length width)) (width-is-one (= 1 width)))
          (if (<= index last-index)
            (let
              (sub-vector
                (if width-is-one (vector-ref a index) (vector-range a index (- (+ width index) 1))))
              (if (set-contains? known sub-vector)
                (loop known width (+ 1 index) last-index width-is-one)
                (begin (set-add! known sub-vector)
                  (+ 1 (loop known width (+ 1 index) last-index width-is-one)))))
            (if (< min-width width)
              (let (width (- width 1))
                (loop (vector-distinct-set-create a-length width) width
                  0 (- a-length width) (= 1 width)))
              0)))))))

(define* (vector-distinct-stream a #:optional (min-width 1) max-width)
  "vector [integer integer] -> stream
   return a stream of all distinct sub-vectors in a vector with lengths from min-width to max-width.
   top to bottom.
   distinctness is defined as with vector-distinct-count"
  "implementation is almost identical to vector-distinct-count"
  (let* ((a-length (vector-length a)) (width (or max-width a-length)))
    (stream-let next
      ( (known (vector-distinct-set-create a-length width)) (width width) (index 0)
        (last-index (- a-length width)) (width-is-one (= 1 width)))
      (if (<= index last-index)
        (let
          (sub-vector
            (if width-is-one (vector-ref a index) (vector-range a index (- (+ width index) 1))))
          (if (set-contains? known sub-vector)
            (next known width (+ 1 index) last-index width-is-one)
            (begin (set-add! known sub-vector)
              (stream-cons sub-vector (next known width (+ 1 index) last-index width-is-one)))))
        (if (< min-width width)
          (let (width (- width 1))
            (next (vector-distinct-set-create a-length width) width
              0 (- a-length width) (= 1 width)))
          stream-null)))))

(define (divisions count)
  "integer -> ((integer ...) ...)
   return all integer selections that sum to count.
   distinctness is defined by length, order and value.
   example for count 3: ((1 1 1) (1 2) (2 1) (3))"
  "algorithm:\n     collect all possible values and associate a rest value that following values have to sum to.\n     for each of those associations, reduce the rest value and the list of possible values while producing possible tails.\n     the rest values are removed after all combinations have been found"
  (define (produce-chain parts rest size)
    (let
      (parts (filter-map (l (a) (and (<= (tail a) rest) (pair (- (first a) size) (tail a)))) parts))
      (apply append
        (map
          (l (a)
            (let (chain (produce-chain parts (first a) (tail a)))
              (if (null? chain) (list (list a)) (produce pair (list a) chain))))
          parts))))
  (let
    (parts
      (map-integers count (l (n) "integer -> (rest . value)" (pair (- count (+ 1 n)) (+ 1 n)))))
    (map-map tail (produce-chain parts count 0))))

(define (number-divisions b count)
  "number integer -> ((number ...) ...)
   return all selections of multiples of b divided by count that sum to b.
   distinctness is defined by length, order and value.
   example
     b: 60, count: 3
     result: ((20 20 20) (20 40) (40 20) (60))"
  (map-map (l (c) (* c (/ b count))) (divisions count)))
