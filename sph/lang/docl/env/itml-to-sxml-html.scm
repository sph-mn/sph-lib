(library (sph lang docl env itml-to-sxml-html)
  (export
    escape
    scm
    sxml
    text-reverse)
  (import
    (guile)
    (rnrs base)
    (sph)
    (only (sph lang indent-syntax) prefix-tree->indent-tree-string))

  (define (sxml nesting-depth docl-state . a) (first a))
  (define (scm nesting-depth docl-state . a) (first a))
  (define (text-reverse nesting-depth docl-state . a) (pair (q div) (reverse a)))

  (define (escape nesting-depth docl-state . a)
    (debug-log a)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n"))))
