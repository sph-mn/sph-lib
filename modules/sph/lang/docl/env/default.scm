(library (sph lang docl env default)
  (export
    *
    +
    -
    /
    and
    docl-param-ref
    l
    list
    or
    qq
    quote
    string-append
    unquote)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist))

  (define-syntax-rule (docl-param-ref docl-state keys ...)
    (alists-q-ref (tail docl-state) keys ...)))
