(library (sph hashtable one)
  (export
    flat-alist-tree->hashtable)
  (import
    (rnrs base)
    (sph)
    (sph hashtable)
    (only (guile) compose)
    (only (sph alist) list->alist)
    (only (sph tree) tree-map-lists-and-self))

  (define (flat-alist-tree->hashtable arg)
    (tree-map-lists-and-self (compose alist->hashtable list->alist) arg)))
