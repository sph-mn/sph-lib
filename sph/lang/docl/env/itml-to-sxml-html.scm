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

  (define (sxml a nesting-depth docl-state) a)
  (define (scm a nesting-depth docl-state) a)
  (define (text-reverse a nesting-depth docl-state) (pair (q div) (reverse a)))

  (define (escape a nesting-depth docl-state)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n"))))
