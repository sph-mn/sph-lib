(library (sph lang docl env itml-to-text)
  (export
    escape)
  (import
    (rnrs base)
    (sph)
    (sph lang indent-syntax)
    (only (guile) string-join))

  (define (escape a nesting-depth docl-state)
    (string-join
      (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e) nesting-depth) e)) a) "\n")))
