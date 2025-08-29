(library (test sph documentation)
  (export
    a
    t
    f
    ht-values)
  (import
    (guile)
    (rnrs base)
    (sph))

  (define* (a b #:optional c d #:rest e)
    "testa testb [else this ...] -> result
    documentation"
    #t)

  (define-syntax-rule (f (g ...) h ...) #t)

  (define (ht-values arg) "hashtable -> vector"
    (call-with-values (l () (ht-entries arg)) (l (keys values) values)))

  (define (t)
    #t))