(library (test sph documentation)
  (export
    a
    t
    f
    hashtable-values)
  (import
    (guile)
    (rnrs base)
    (sph))

  (define* (a b #:optional c d #:rest e)
    "testa testb [else this ...] -> result
    documentation"
    #t)

  (define-syntax-rule (f (g ...) h ...) #t)

  (define (hashtable-values arg) "hashtable -> vector"
    (call-with-values (l () (hashtable-entries arg)) (l (keys values) values)))

  (define (t)
    #t))