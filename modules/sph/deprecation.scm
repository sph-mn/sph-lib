(library (sph deprecation)
  (export
    set-deprecated
    sph-deprecation-description)
  (import
    (only (rnrs base) set!)
    (sph)
    (only (guile) current-error-port simple-format))

  (define sph-deprecation-description
    "display a deprecation warning when specific procedures are used. experimental")

  (define (deprecation-warning-proc proc)
    (l a (simple-format (current-error-port) "deprecated procedure used ~S\n" proc) (apply proc a)))

  (define-syntax-rule (set-deprecated proc ...)
    ;set a list of procedures as deprecated
    (begin (set! proc (deprecation-warning-proc proc)) ...)))
