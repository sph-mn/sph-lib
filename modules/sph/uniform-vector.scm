(library (sph uniform-vector)
  (export
    f32vector-copy
    f32vector-create
    f32vector-each-index
    f32vector-map
    f32vector-map+one
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

  (define (f32vector-copy a) "http://lists.gnu.org/archive/html/bug-guile/2014-10/msg00024.html"
    (let ((r (make-f32vector (f32vector-length a))))
      (bytevector-copy! a 0 r 0 (* 4 (f32vector-length a))) r))

  (define (f32vector-each-index proc a) "{integer:index integer:vector-a-length ->} f32vector ->"
    (let (length (f32vector-length a))
      (let loop ((index 0)) (if (< index length) (begin (proc index length) (loop (+ 1 index)))))))

  (define (f32vector-range-map! proc start end r a . b)
    "{any:element ... -> any} integer:index integer:index f32vector:result f32vector ... -> f32vector"
    (let loop ((index start))
      (if (<= index end)
        (begin
          (f32vector-set! r index
            (apply proc (f32vector-ref a index) (map (l (e) (f32vector-ref e index)) b)))
          (loop (+ 1 index)))
        r)))

  (define (f32vector-range-map proc start end a . b)
    "{any:element ... -> any} integer:index integer:index f32vector ... -> f32vector"
    (apply f32vector-range-map! proc start end (make-f32vector (f32vector-length a)) a b))

  (define (f32vector-map proc a . b)
    "procedure:{any:element ... -> any} f32vector ... -> f32vector
     can easily build processors like f32vector-sum: (f32vector-map + a b c)"
    (let (a-length (f32vector-length a))
      (apply f32vector-range-map! proc 0 (- a-length 1) (make-f32vector a-length) a b)))

  (define (f32vector-create length proc)
    "integer {index -> float} -> f32vector
     make and initialise an f32vector"
    (let (r (make-f32vector length))
      (let loop ((index 0))
        (if (< index length) (begin (f32vector-set! r index (proc index)) (loop (+ 1 index))) r))))

  (define (f32vector-map+one proc variable . a)
    "{any:variable any:element ... -> any} any:variable f32vector -> f32vector
     like f32vector-map but passes the given variable as an additional first argument on each call to \"proc\""
    (apply f32vector-map (l e (apply proc variable e)) a)))
