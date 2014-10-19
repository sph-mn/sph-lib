(library (sph deprecation)
  (export
    set-deprecated)
  (import
    (rnrs base)
    (sph)
    (only (guile) current-error-port simple-format))

  (define (deprecation-warning-proc proc)
    (l args (simple-format (current-error-port) "deprecated procedure used ~S\n" proc)
      (apply proc args)))

  (define-syntax-rule (set-deprecated proc ...)
    (begin (set! proc (deprecation-warning-proc proc)) ...)))