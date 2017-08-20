(library (sph lang itml eval env shtml)
  (export
    escape
    itml-param-insert
    scm-eval
    scm-qq)
  (import
    (guile)
    (sph)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph list)
    (sph string))

  (define-syntax-rule (scm-qq state a ...) (list-qq (a ...)))
  (define-syntax-rule (scm-eval state a ...) (list a ...))
  (define-syntax-rule (escape state a ...) (list (q pre) (prefix-tree->indent-tree (q ((a ...)))))))
