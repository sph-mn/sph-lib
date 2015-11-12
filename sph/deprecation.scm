;sets up procedures so that a deprecation warning is written to standard-error when they are used

(library (sph deprecation)
  (export
    set-deprecated)
  (import
    (rnrs base)
    (sph)
    (only (guile) current-error-port simple-format))

  (define (deprecation-warning-proc proc)
    (l a (simple-format (current-error-port) "deprecated procedure used ~S\n" proc)
      (apply proc a)))

  (define-syntax-rule (set-deprecated proc ...)
    (begin (set! proc (deprecation-warning-proc proc)) ...)))
