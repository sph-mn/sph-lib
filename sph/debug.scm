(library (sph debug)
  (export
    pass
    trace
    trace-in)
  (import
    (guile)
    (ice-9 pretty-print)
    (rnrs base)
    (only (guile) basename simple-format)
    (only (sph one) pass)
    (sph)
    (system vm program)
    (system vm trace)
    (system vm vm))

  (define (trace . procs)
    (each
      (l (ele) (trace-calls-to-procedure ele #:width 200 #:prefix "")) procs))

  (define (trace-in . procs)
    (each
      (l (ele) (trace-calls-in-procedure ele #:width 200 #:prefix "")) procs))

  (set-vm-trace-level! (the-vm) 1)
  (debug-set! show-file-name (quote base)))