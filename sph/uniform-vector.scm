(library (sph uniform-vector)
  (export
    f32vector-difference!
    f32vector-division!
    f32vector-each-index
    f32vector-map!
    f32vector-map+one!
    f32vector-map-fold!
    f32vector-product!
    f32vector-sum!)
  (import
    (rnrs base)
    (sph)
    (srfi srfi-4))

  (define (f32vector-each-index proc a) "{integer:index integer:vector-a-length ->} f32vector ->"
    (let (length (f32vector-length a))
      (let loop ((index 0)) (if (< index length) (begin (proc index length) (loop (+ 1 index)))))))

  (define (f32vector-map! proc r)
    "{any:element integer:index integer:vector-a-length -> any} f32vector ->"
    (f32vector-each-index
      (l (index length) (f32vector-set! r index (proc (f32vector-ref index) index length))) r))

  (define (f32vector-map+one! proc variable a)
    "{any:element any:variable -> any} any:variable f32vector
    like f32vector-map! but passes the given variable as a second argument on each call to \"proc\""
    (f32vector-map (l (e index length) (proc e variable)) a))

  (define (f32vector-map-fold! proc a . b)
    "procedure:{any:b-element any:previous-result-or-a-element} f32vector ... ->
    update each element of vector a with the result of calling proc with an element at
    the current index from and for every vector b, and an value that starts with the current element of vector a and
    is updated with the result of calling proc.
    overwrites and maps vector a and for each element folds vector b to create the current map result"
    (f32vector-each-index
      (l (index length)
        (f32vector-set! a index
          (fold (l (e r) (proc (f32vector-ref b index) r)) (f32vector-ref a index) b)))
      a))

  (define (f32vector-sum! a . b)
    "f32vector ... ->
    adds every value in vector a with all values at the same index from vectors b"
    (apply f32vector-map-fold! + a b))

  (define (f32vector-difference! a . b)
    "f32vector ... ->
    subtracts every value in vector a with all values at the same index from vectors b"
    (apply f32vector-map-fold! - a b))

  (define (f32vector-division! a . b)
    "f32vector ... ->
    divised every value in vector a with all values at the same index from vectors b"
    (apply f32vector-map-fold! / a b))

  (define (f32vector-product! a . b)
    "f32vector ... ->
    multiplies every value in vector a with all values at the same index from vectors b"
    (apply f32vector-map-fold! * a b)))