(library (sph uniform-vector)
  (export
    bytevector-append
    bytevector-contains?
    f32vector-copy
    f32vector-copy*
    f32vector-copy-zero
    f32vector-copy-zero*
    f32vector-create
    f32vector-each-index
    f32vector-map
    f32vector-map!
    f32vector-map-with
    f32vector-map-with!
    f32vector-map-with-index
    f32vector-map-with-index!
    f32vector-range-map
    f32vector-range-map!
    f32vector-range-map-with
    f32vector-range-map-with!
    f32vector-range-map-with-index!
    f32vector-range-set
    f32vector-range-set!
    f64vector-copy
    f64vector-copy*
    f64vector-copy-zero
    f64vector-copy-zero*
    f64vector-create
    f64vector-each-index
    f64vector-map
    f64vector-map!
    f64vector-map-with
    f64vector-map-with!
    f64vector-map-with-index
    f64vector-map-with-index!
    f64vector-range-map
    f64vector-range-map!
    f64vector-range-map-with
    f64vector-range-map-with!
    f64vector-range-map-with-index!
    f64vector-range-set
    f64vector-range-set!
    integer->bytevector
    s16vector-copy
    s16vector-copy*
    s16vector-copy-zero
    s16vector-copy-zero*
    s16vector-create
    s16vector-each-index
    s16vector-map
    s16vector-map!
    s16vector-map-with
    s16vector-map-with!
    s16vector-range-map
    s16vector-range-map!
    s16vector-range-map-with
    s16vector-range-map-with!
    s16vector-range-set
    s16vector-range-set!
    s32vector-copy
    s32vector-copy*
    s32vector-copy-zero
    s32vector-copy-zero*
    s32vector-create
    s32vector-each-index
    s32vector-map
    s32vector-map!
    s32vector-map-with
    s32vector-map-with!
    s32vector-range-map
    s32vector-range-map!
    s32vector-range-map-with
    s32vector-range-map-with!
    s32vector-range-set
    s32vector-range-set!
    s8vector-copy
    s8vector-copy*
    s8vector-copy-zero
    s8vector-copy-zero*
    s8vector-create
    s8vector-each-index
    s8vector-map
    s8vector-map!
    s8vector-map-with
    s8vector-map-with!
    s8vector-range-map
    s8vector-range-map!
    s8vector-range-map-with
    s8vector-range-map-with!
    s8vector-range-set
    s8vector-range-set!
    sph-uniform-vector-description)
  (import
    (guile)
    (rnrs base)
    (rnrs bytevectors)
    (sph)
    (sph number)
    (srfi srfi-4))

  (define sph-uniform-vector-description
    "helpers for srfi-4 and compatible vectors. for example f32vector")

  (define-syntax-rule (define-uv-copy id make-vector vector-length byte-size)
    (define (id a) "xvector -> xvector"
      ; bytevector-copy did not work. http://lists.gnu.org/archive/html/bug-guile/2014-10/msg00024.html
      (let (result (make-vector (vector-length a)))
        (bytevector-copy! a 0 result 0 (* byte-size (vector-length a))) result)))

  (define-syntax-rule (define-uv-copy-zero id make-vector vector-length)
    (define (id a) (make-vector (vector-length a) 0)))

  (define-syntax-rule (define-uv-copy* id vector-copy)
    (define (id a c) (let (result (vector-copy a)) (c result) result)))

  (define-syntax-rule (define-uv-copy-zero* id vector-copy-zero)
    (define (id a c) (let (result (vector-copy-zero a)) (c result) result)))

  (define-syntax-rule (define-uv-each-index id vector-length)
    (define (id f a)
      "procedure:{integer integer:a-length -> unspecified} xvector -> unspecified
       call f for each element in \"a\" with the index of the current element in \"a\" and the size of \"a\""
      (let (length (vector-length a))
        (let loop ((index 0)) (if (< index length) (begin (f index length) (loop (+ 1 index))))))))

  (define-syntax-rule (define-uv-range-map! id vector-set! vector-ref)
    (define (id result f start end a . b)
      "procedure:{any ... -> any} integer integer xvector xvector:source ... -> unspecified
       set result to the map results of calling f for each element between start and end of one or multiple xvectors.
       f is called (f a-element b-element ...).
       all vectors must be of sufficient size"
      (let loop ((index start))
        (if (<= index end)
          (begin
            (vector-set! result index
              (apply f (vector-ref a index) (map (l (a) (vector-ref a index)) b)))
            (loop (+ 1 index)))))))

  (define-syntax-rule (define-uv-range-set! id vector-set!)
    (define (id a f start end)
      (let loop ((index start))
        (if (<= index end) (begin (vector-set! a index (f index)) (loop (+ 1 index)))))))

  (define-syntax-rule (define-uv-range-set id vector-copy* vector-range-set!)
    (define (id a f start end) (vector-copy* a (l (result) (vector-range-set! result f start end)))))

  (define-syntax-rule (define-uv-map! id vector-length vector-range-map!)
    (define (id f a . b)
      "procedure:{any:element ... -> any} xvector ... -> unspecified
       like uv-map but modifies \"a\""
      (apply vector-range-map! a f 0 (- (vector-length a) 1) a b)))

  (define-syntax-rule (define-uv-map-with! id vector-map!)
    (define (id f variable . a)
      "procedure:{any:variable any:element ... -> any} any:variable xvector -> unspecified
       like xvector-map but passes the given variable as an additional first argument on each call to f.
       example call: (vector-map-with! * 2 a)"
      (apply vector-map! (l a (apply f variable a)) a)))

  (define-syntax-rule (define-uv-range-map-with! id vector-range-map!)
    (define (id result f variable start end a . b)
      (apply vector-range-map! result (l a (apply f variable a)) start end a b)))

  (define-syntax-rule (define-uv-range-map id vector-copy-zero* vector-range-map!)
    (define (id f start end a . b)
      "procedure:{any ... -> any} integer integer xvector ... -> xvector
       like xvector-range-map but does not modify input"
      (vector-copy-zero* a (l (result) (apply vector-range-map! result f start end a b)))))

  (define-syntax-rule (define-uv-map id vector-copy-zero* vector-range-map! vector-length)
    (define (id f a . b)
      "procedure:{any:element ... -> any} xvector ... -> xvector
       call f for each element of each vector. (f a-element b-element ...)
       can be used to create processors like xvector-sum: (xvector-map + a b c)"
      (vector-copy-zero* a
        (l (result) (apply vector-range-map! result f 0 (- (vector-length a) 1) a b)))))

  (define-syntax-rule (define-uv-create id make-vector vector-set!)
    (define (id length f)
      "integer {index -> float} -> xvector
       make and initialise an xvector with the results of calling f as (f index)"
      (let (result (make-vector length))
        (let loop ((index 0))
          (if (< index length) (begin (vector-set! result index (f index)) (loop (+ 1 index)))
            result)))))

  (define-syntax-rule (define-uv-map-with id vector-map)
    (define (id f variable . a)
      "procedure:{any:variable any:element ... -> any} any:variable xvector -> xvector
       like xvector-map but passes the given variable as an additional first argument to each call of f.
       example call: (xvector-map-with * 2 a)"
      (apply vector-map (l a (apply f variable a)) a)))

  (define-syntax-rule (define-uv-range-map-with id vector-range-map)
    (define (id f variable start end a . b)
      (apply vector-range-map (l a (apply f variable a)) start end a b)))

  (define-syntax-rule (define-uv-map-with-index! id vector-length vector-range-map!)
    (define (id f a . b)
      "procedure:{any:element ... -> any} xvector ... -> unspecified
       like uv-map but modifies \"a\""
      (apply vector-range-map! a f 0 (- (vector-length a) 1) a b)))

  (define-syntax-rule
    (define-uv-map-with-index id vector-copy-zero* vector-range-map! vector-length)
    (define (id f a . b)
      "procedure:{index any:element ... -> any} xvector ... -> xvector
       call f for each element of each vector. (f index a-element b-element ...)"
      (vector-copy-zero* a
        (l (result) (apply vector-range-map! result f 0 (- (vector-length a) 1) a b)))))

  (define-syntax-rule (define-uv-range-map-with-index! id vector-set! vector-ref)
    (define (id result f start end a . b)
      "procedure:{integer:index any:element ... -> any} integer integer xvector xvector:source ... -> unspecified
       set result to the map results of calling f for each element between start and end of one or multiple xvectors.
       f is called (f index a-element b-element ...).
       all vectors must be of sufficient size"
      (let loop ((index start))
        (if (<= index end)
          (begin
            (vector-set! result index
              (apply f index (vector-ref a index) (map (l (a) (vector-ref a index)) b)))
            (loop (+ 1 index)))))))

  ; f64
  (define-uv-copy f64vector-copy make-f64vector f64vector-length 8)
  (define-uv-copy-zero f64vector-copy-zero make-f64vector f64vector-length)
  (define-uv-copy* f64vector-copy* f64vector-copy)
  (define-uv-copy-zero* f64vector-copy-zero* f64vector-copy-zero)
  (define-uv-each-index f64vector-each-index f64vector-length)
  (define-uv-range-map! f64vector-range-map! f64vector-set! f64vector-ref)
  (define-uv-map! f64vector-map! f64vector-length f64vector-range-map!)
  (define-uv-map-with! f64vector-map-with! f64vector-map!)
  (define-uv-range-map f64vector-range-map f64vector-copy-zero* f64vector-range-map!)
  (define-uv-map f64vector-map f64vector-copy-zero* f64vector-range-map! f64vector-length)
  (define-uv-create f64vector-create make-f64vector f64vector-set!)
  (define-uv-map-with f64vector-map-with f64vector-map)
  (define-uv-range-map-with f64vector-range-map-with f64vector-range-map)
  (define-uv-range-map-with! f64vector-range-map-with! f64vector-range-map!)
  (define-uv-range-set! f64vector-range-set! f64vector-set!)
  (define-uv-range-set f64vector-range-set f64vector-copy* f64vector-range-set!)

  (define-uv-map-with-index! f64vector-map-with-index! f64vector-length
    f64vector-range-map-with-index!)

  (define-uv-map-with-index f64vector-map-with-index f64vector-copy-zero*
    f64vector-range-map-with-index! f64vector-length)

  (define-uv-range-map-with-index! f64vector-range-map-with-index! f64vector-set! f64vector-ref)
  ;
  ; f32
  (define-uv-copy f32vector-copy make-f32vector f32vector-length 4)
  (define-uv-copy-zero f32vector-copy-zero make-f32vector f32vector-length)
  (define-uv-copy* f32vector-copy* f32vector-copy)
  (define-uv-copy-zero* f32vector-copy-zero* f32vector-copy-zero)
  (define-uv-each-index f32vector-each-index f32vector-length)
  (define-uv-range-map! f32vector-range-map! f32vector-set! f32vector-ref)
  (define-uv-map! f32vector-map! f32vector-length f32vector-range-map!)
  (define-uv-map-with! f32vector-map-with! f32vector-map!)
  (define-uv-range-map f32vector-range-map f32vector-copy-zero* f32vector-range-map!)
  (define-uv-map f32vector-map f32vector-copy-zero* f32vector-range-map! f32vector-length)
  (define-uv-create f32vector-create make-f32vector f32vector-set!)
  (define-uv-map-with f32vector-map-with f32vector-map)
  (define-uv-range-map-with f32vector-range-map-with f32vector-range-map)
  (define-uv-range-map-with! f32vector-range-map-with! f32vector-range-map!)
  (define-uv-range-set! f32vector-range-set! f32vector-set!)
  (define-uv-range-set f32vector-range-set f32vector-copy* f32vector-range-set!)

  (define-uv-map-with-index! f32vector-map-with-index! f32vector-length
    f32vector-range-map-with-index!)

  (define-uv-map-with-index f32vector-map-with-index f32vector-copy-zero*
    f32vector-range-map-with-index! f32vector-length)

  (define-uv-range-map-with-index! f32vector-range-map-with-index! f32vector-set! f32vector-ref)
  ;
  ; s32
  (define-uv-copy s32vector-copy make-s32vector s32vector-length 4)
  (define-uv-copy-zero s32vector-copy-zero make-s32vector s32vector-length)
  (define-uv-copy* s32vector-copy* s32vector-copy)
  (define-uv-copy-zero* s32vector-copy-zero* s32vector-copy-zero)
  (define-uv-each-index s32vector-each-index s32vector-length)
  (define-uv-range-map! s32vector-range-map! s32vector-set! s32vector-ref)
  (define-uv-map! s32vector-map! s32vector-length s32vector-range-map!)
  (define-uv-map-with! s32vector-map-with! s32vector-map!)
  (define-uv-range-map s32vector-range-map s32vector-copy-zero* s32vector-range-map!)
  (define-uv-map s32vector-map s32vector-copy-zero* s32vector-range-map! s32vector-length)
  (define-uv-create s32vector-create make-s32vector s32vector-set!)
  (define-uv-map-with s32vector-map-with s32vector-map)
  (define-uv-range-map-with s32vector-range-map-with s32vector-range-map)
  (define-uv-range-map-with! s32vector-range-map-with! s32vector-range-map!)
  (define-uv-range-set! s32vector-range-set! s32vector-set!)
  (define-uv-range-set s32vector-range-set s32vector-copy* s32vector-range-set!)
  ;
  ; s16
  (define-uv-copy s16vector-copy make-s16vector s16vector-length 2)
  (define-uv-copy-zero s16vector-copy-zero make-s16vector s16vector-length)
  (define-uv-copy* s16vector-copy* s16vector-copy)
  (define-uv-copy-zero* s16vector-copy-zero* s16vector-copy-zero)
  (define-uv-each-index s16vector-each-index s16vector-length)
  (define-uv-range-map! s16vector-range-map! s16vector-set! s16vector-ref)
  (define-uv-map! s16vector-map! s16vector-length s16vector-range-map!)
  (define-uv-map-with! s16vector-map-with! s16vector-map!)
  (define-uv-range-map s16vector-range-map s16vector-copy-zero* s16vector-range-map!)
  (define-uv-map s16vector-map s16vector-copy-zero* s16vector-range-map! s16vector-length)
  (define-uv-create s16vector-create make-s16vector s16vector-set!)
  (define-uv-map-with s16vector-map-with s16vector-map)
  (define-uv-range-map-with s16vector-range-map-with s16vector-range-map)
  (define-uv-range-map-with! s16vector-range-map-with! s16vector-range-map!)
  (define-uv-range-set! s16vector-range-set! s16vector-set!)
  (define-uv-range-set s16vector-range-set s16vector-copy* s16vector-range-set!)
  ;
  ; s8
  (define-uv-copy s8vector-copy make-s8vector s8vector-length 1)
  (define-uv-copy-zero s8vector-copy-zero make-s8vector s8vector-length)
  (define-uv-copy* s8vector-copy* s8vector-copy)
  (define-uv-copy-zero* s8vector-copy-zero* s8vector-copy-zero)
  (define-uv-each-index s8vector-each-index s8vector-length)
  (define-uv-range-map! s8vector-range-map! s8vector-set! s8vector-ref)
  (define-uv-map! s8vector-map! s8vector-length s8vector-range-map!)
  (define-uv-map-with! s8vector-map-with! s8vector-map!)
  (define-uv-range-map s8vector-range-map s8vector-copy-zero* s8vector-range-map!)
  (define-uv-map s8vector-map s8vector-copy-zero* s8vector-range-map! s8vector-length)
  (define-uv-create s8vector-create make-s8vector s8vector-set!)
  (define-uv-map-with s8vector-map-with s8vector-map)
  (define-uv-range-map-with s8vector-range-map-with s8vector-range-map)
  (define-uv-range-map-with! s8vector-range-map-with! s8vector-range-map!)
  (define-uv-range-set! s8vector-range-set! s8vector-set!)
  (define-uv-range-set s8vector-range-set s8vector-copy* s8vector-range-set!)

  (define (integer->bytevector a)
    "integer:signed-integer -> bytevector
     create a bytevector of minimum size storing the given signed integer"
    (let*
      ( (size (bit->byte-length (+ 1 (number-container-length (abs a) 2))))
        (r (make-bytevector size)))
      size (bytevector-sint-set! r 0 a (native-endianness) size) r))

  (define (bytevector-append . a) "bytevector ... -> bytevector"
    (let (r (make-bytevector (fold (l (e prev) (+ prev (bytevector-length e))) 0 a)))
      (fold
        (l (e index)
          (let (len (bytevector-length e)) (bytevector-copy! e 0 r index len) (+ index len)))
        0 a)
      r))

  (define (bytevector-contains? a search-bv)
    "bytevector bytevector -> boolean
     true if bytevector \"a\" contains bytevector \"search-bv\""
    (let ((a-length (bytevector-length a)) (search-bv-length (bytevector-length search-bv)))
      (if (> search-bv-length a-length) #f
        (let
          ( (search (list->vector (bytevector->u8-list search-bv)))
            (last-match-index (- search-bv-length 1)))
          (let loop ((index 0) (match-index 0))
            (if (< index a-length)
              (if (= (bytevector-u8-ref a index) (vector-ref search match-index))
                (if (= last-match-index match-index) #t (loop (+ 1 index) (+ 1 match-index)))
                (loop (+ 1 index) 0))
              #f)))))))
