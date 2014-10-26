(library (sph uniform-vector)
  (export
    f32vector-copy
    f32vector-difference
    f32vector-division
    f32vector-each-index
    f32vector-map
    f32vector-map+one
    f32vector-map-fold!
    f32vector-product
    f32vector-sum)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-4)
    (only (rnrs bytevectors) bytevector-copy! bytevector=?))

  (define (f32vector-copy a)
    (let ((r (make-f32vector (f32vector-length a))))
      (bytevector-copy! a 0 r 0 (* 4 (f32vector-length a))) r))

  (define (f32vector-each-index proc a) "{integer:index integer:vector-a-length ->} f32vector ->"
    (let (length (f32vector-length a))
      (let loop ((index 0)) (if (< index length) (begin (proc index length) (loop (+ 1 index)))))))

  (define (f32vector-map-fold! proc a . b)
    "procedure:{any:b-element any:previous-result-or-a-element} f32vector ... ->
    update each element of vector a with the result of calling proc with an element at
    the current index from and for every vector b, and an value that starts with the current element of vector a and
    is updated with the result of calling proc.
    overwrites and maps vector a and for each element folds vector b to create the current map result"
    (f32vector-each-index
      (l (index length)
        (f32vector-set! a index
          (fold (l (e r) (proc (f32vector-ref e index) r)) (f32vector-ref a index) b)))
      a))

  (define (f32vector-map proc a)
    "{any:element integer:index integer:vector-a-length -> any} f32vector -> f32vector"
    (let (r (f32vector-copy a))
      (f32vector-each-index
        (l (index length) (f32vector-set! r index (proc (f32vector-ref r index) index length))) r)
      r))

  (define (f32vector-map+one proc variable a)
    "{any:element any:variable -> any} any:variable f32vector -> f32vector
    like f32vector-map but passes the given variable as a second argument on each call to \"proc\""
    (f32vector-map (l (e index length) (proc e variable)) a))

  (define (f32vector-sum a . b)
    "f32vector ... -> f32vector
    add all values of vectors b to values of vector a at the same index "
    (let (r (f32vector-copy a)) (apply f32vector-map-fold! + r b) r))

  (define (f32vector-difference a . b)
    "f32vector ... -> f32vector
    subtract all values of vectors b from values of vector a at the same index"
    (let (r (f32vector-copy a)) (apply f32vector-map-fold! - r b) r))

  (define (f32vector-product a . b)
    "f32vector ... -> f32vector
    multiply all values of vectors b with values of vector a at the same index"
    (let (r (f32vector-copy a)) (apply f32vector-map-fold! * r b) r))

  (define (f32vector-division a . b)
    "f32vector ... -> f32vector
    divide all values of vectors b with values of vector a at the same index"
    (let (r (f32vector-copy a)) (apply f32vector-map-fold! / r b) r)))