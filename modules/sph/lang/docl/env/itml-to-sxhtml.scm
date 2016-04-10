(library (sph lang docl env itml-to-sxhtml)
  (export
    escape
    scm
    sxml)
  (import
    (guile)
    (rnrs base)
    (sph)
    (only (sph lang indent-syntax) prefix-tree->indent-tree-string))

  (define-syntax-rule (sxml nesting-depth docl-state a ...) (qq (a ...)))

  (define (scm nesting-depth docl-state . a) (first a))

  (define (escape nesting-depth docl-state . a)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n"))))
