(library (sph lang docl env itml-to-text)
  (export
    escape)
  (import
    (rnrs base)
    (sph)
    (sph lang docl env)
    (sph lang indent-syntax)
    (only (guile) string-join))

  (define (escape a)
    (let (nesting-level (current-nesting-level))
      (string-join
        (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e) nesting-level) e)) a)
        "\n"))))
