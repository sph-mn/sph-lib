(library (sph tree two)
  (export
    tree->values-count-hash
    tree-replace-from-hashtable)
  (import
    (rnrs base)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph tree))

  (define (tree->values-count-hash a)
    (let (r (hashtable))
      (tree-each-leaf
        (l (e)
          (let (existing (ht-ref r e))
            (if existing (ht-set! r e (+ existing 1)) (ht-set! r e 1))))
        a)))

  (define (tree-replace-from-hashtable a ht) "list rnrs-hashtable -> list/any"
    (if (list? a) (map (l (e) (tree-replace-from-hashtable e ht)) a)
      (identity-if (ht-ref ht a) a))))
