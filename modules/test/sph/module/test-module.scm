(library (test sph module test-module)
  (export
    execute)
  (import
    (sph)
    (rnrs base)
    (sph test))

  (define (execute settings) (list "execute called")))
