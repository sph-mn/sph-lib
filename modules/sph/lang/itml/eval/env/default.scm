(library (sph lang itml eval env default)
  (export
    *
    +
    -
    /
    and
    itml-param-ref
    l
    lambda
    list
    or
    qq
    quote
    string-append
    unquote)
  (import
    (guile)
    (sph)
    (sph hashtable))

  (define-syntax-rule (itml-param-ref state key ...)
    (ht-tree-ref-q (itml-state-data data) param key ...)))
