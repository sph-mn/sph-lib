(library (sph tree two)
  (export
    tree->values-count-hash
    tree-replace-from-hashtable)
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
        a)))

  (define (tree-replace-from-hashtable a ht) "list rnrs-hashtable -> list/any"
    (if (list? a) (map (l (e) (tree-replace-from-hashtable e ht)) a)
      (identity-if (hashtable-ref ht a) a))))
