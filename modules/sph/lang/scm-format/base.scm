(library (sph lang scm-format base)
  (export
    comment?)
  (import
    (rnrs base)
    (sph))

  (define (comment? a)
    (and (list? a) (not (null? a))
      (or (eqv? (q semicolon-comment) (first a)) (eqv? (q range-comment) (first a))))))
