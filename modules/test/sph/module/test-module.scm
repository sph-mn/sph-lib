(library (test sph module test-module)
  (export
    execute)
  (import
    (sph common)
    ;(rnrs base)
    (sph test))

  (define (execute settings) (debug-log "execute called") #t))
