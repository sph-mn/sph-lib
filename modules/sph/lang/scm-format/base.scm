(define-module (sph lang scm-format base))
(export comment?)
(use-modules (sph))

(define (comment? a)
  (and (list? a) (not (null? a))
    (or (eqv? (q semicolon-comment) (first a)) (eqv? (q range-comment) (first a)))))
