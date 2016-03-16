(library (sph tree two)
  (export
    tree->values-count-hash)
  (import
    (rnrs base)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph tree)
    (only (rnrs hashtables) hashtable-set!))

  (define (tree->values-count-hash a)
    (let (r (hashtable))
      (tree-each-leaf
        (l (e)
          (let (existing (hashtable-ref r e))
            (if existing (hashtable-set! r e (+ existing 1)) (hashtable-set! r e 1))))
        a))))
