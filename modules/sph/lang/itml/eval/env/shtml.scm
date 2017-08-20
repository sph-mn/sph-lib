(library (sph lang itml eval env shtml)
  (export
    escape
    itml-param-insert
    scm-eval
    scm-qq)
  (import
    (guile)
    (sph)
    (sph list)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph string))

  (define-syntax-rule (scm-qq state a ...) (list-qq a ...))
  (define-syntax-rule (scm-eval state a ...) (begin a ...))

  (define (escape state . a)
    (list (q pre)
      (string-join (map (l (a) (if (list? a) (prefix-tree->indent-tree (list a)) a)) a) "\n"))))
