(library (sph lang itml eval env shtml)
  (export
    escape
    itml-param-insert
    scm-eval
    scm-list-qq)
  (import
    (guile)
    (sph)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph string))

  (define-syntax-rule (scm-list-qq state a ...) (qq (a ...)))
  (define-syntax-rule (scm-eval state a ...) (begin a ...))

  (define-syntax-rule (itml-param-insert state key ...)
    (or (itml-param-ref state key ...) (string-join (map any->string (q (key ...))) " ")))

  (define (escape state . a)
    (list (q pre)
      (string-join (map (l (a) (if (list? a) (prefix-tree->indent-tree-string (list a)) a)) a) "\n"))))
