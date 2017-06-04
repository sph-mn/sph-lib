(library (sph debug)
  (export
    pass
    sph-debug-description
    trace
    trace-in)
  (import
    (guile)
    (ice-9 pretty-print)
    (rnrs base)
    (sph)
    (system vm program)
    (system vm trace)
    (system vm vm)
    (only (guile) basename simple-format)
    (only (sph one) pass))

  (define sph-debug-description "debugging helpers. experimental")

  (define (trace . procs)
    (each (l (ele) (trace-calls-to-procedure ele #:width 200 #:prefix "")) procs))

  (define (trace-in . procs)
    (each (l (ele) (trace-calls-in-procedure ele #:width 200 #:prefix "")) procs))

  (set-vm-trace-level! 1)
  (debug-set! show-file-name (q base)))
