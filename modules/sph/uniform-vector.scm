(library (sph uniform-vector)
  (export
    f32vector-copy
    f32vector-copy*
    f32vector-copy-empty
    f32vector-copy-empty*
    f32vector-create
    f32vector-each-index
    f32vector-map
    f32vector-map!
    f32vector-map!*
    f32vector-map*
    f32vector-range-map
    f32vector-range-map!
    sph-uniform-vector-description)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-4)
    (only (rnrs bytevectors) bytevector-copy! bytevector=?))

  (define sph-uniform-vector-description "helpers for srfi-4 and compatible vectors")

  (define (f32vector-copy a) "f32vector -> f32vector"
    ; simple bytevector-copy did not work. bugreport: http://lists.gnu.org/archive/html/bug-guile/2014-10/msg00024.html
    (let (result (make-f32vector (f32vector-length a)))
      (bytevector-copy! a 0 result 0 (* 4 (f32vector-length a))) result))

  (define (f32vector-copy-empty a) (make-f32vector (f32vector-length a)))
  (define (f32vector-copy* a c) (let (result (f32vector-copy a)) (c result) result))
  (define (f32vector-copy-empty* a c) (let (result (f32vector-copy-empty a)) (c result) result))

  (define (f32vector-each-index f a)
    "procedure:{integer integer:a-length -> unspecified} f32vector -> unspecified
     call f for each element in \"a\" with the index of the current element in \"a\" and the size of \"a\""
    (let (length (f32vector-length a))
      (let loop ((index 0)) (if (< index length) (begin (f index length) (loop (+ 1 index)))))))

  (define (f32vector-range-map! f start end result a . b)
    "procedure:{any ... -> any} integer integer f32vector f32vector:source ... -> unspecified
     set result to the map results of calling f for each element of one or multiple f32vectors.
     f is called (f a-element b-element ...).
     all vectors must be of sufficient size"
    (let loop ((index start))
      (if (<= index end)
        (begin
          (f32vector-set! result index
            (apply f (f32vector-ref a index) (map (l (a) (f32vector-ref a index)) b)))
          (loop (+ 1 index))))))

  (define (f32vector-map! f a . b)
    "procedure:{any:element ... -> any} f32vector ... -> unspecified
     like f32vector-map but modifies \"a\""
    (apply f32vector-range-map! f 0 (- (f32vector-length a) 1) a a b))

  (define (f32vector-map!* f variable . a)
    "procedure:{any:variable any:element ... -> any} any:variable f32vector -> unspecified
     like f32vector-map but passes the given variable as an additional first argument on each call to f.
     example call: (f32vector-map* * 2 a)"
    (apply f32vector-map! (l a (apply f variable a)) a))

  (define (f32vector-range-map f start end a . b)
    "procedure:{any ... -> any} integer integer f32vector ... -> f32vector
     like f32vector-range-map but does not modify input"
    (f32vector-copy-empty* a (l (result) (apply f32vector-range-map! f start end result a b))))

  (define (f32vector-map f a . b)
    "procedure:{any:element ... -> any} f32vector ... -> f32vector
     call f for each element of each vector. (f a-element b-element ...)
     can easily build processors like f32vector-sum: (f32vector-map + a b c)"
    (f32vector-copy-empty* a
      (l (result) (apply f32vector-range-map! f 0 (- (f32vector-length a) 1) result a b))))

  (define (f32vector-create length f)
    "integer {index -> float} -> f32vector
     make and initialise an f32vector with the results of calling f as (f index)"
    (let (result (make-f32vector length))
      (let loop ((index 0))
        (if (< index length) (begin (f32vector-set! result index (f index)) (loop (+ 1 index)))
          result))))

  (define (f32vector-map* f variable . a)
    "procedure:{any:variable any:element ... -> any} any:variable f32vector -> f32vector
     like f32vector-map but passes the given variable as an additional first argument on each call to f.
     example call: (f32vector-map* * 2 a)"
    (apply f32vector-map (l a (apply f variable a)) a)))
