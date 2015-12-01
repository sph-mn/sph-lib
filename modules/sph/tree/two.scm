(library (sph tree two)
  (export
    tree->values-count-alist
    tree->values-count-hash)
  (import
    (rnrs base)
    (sph)
    (sph tree)
    (sph alist)
    (sph hashtable))

  (define (tree->values-count-hash arg)
    (let (res (hashtable))
      (tree-each-leaf
        (l (ele)
          (let (existing (hashtable-ref res ele))
            (if existing (hashtable-set! res ele (+ existing 1)) (hashtable-set! res ele 1))))
        arg)))

  (define (tree->values-count-alist arg)
    (tree-fold-leaf
      (l (ele res)
        (let (existing (alist-ref res ele))
          (if existing (alist-set! res ele (+ existing 1)) (alist-set! res ele 1))))
      (list) arg)))