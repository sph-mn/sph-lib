(library (sph lang docl env itml-to-plaintext)
  (export
    escape
    scm)
  (import
    (rnrs base)
    (sph)
    (sph lang indent-syntax)
    (only (guile)
      string-join
      reverse
      negate)
    (only (sph list) map-selected)
    (only (sph string) any->string string-multiply))

  (define (scm nesting-depth docl-state . a)
    (if (null? a) a (if (list? (first a)) (first a) (string-join (map any->string a) " "))))

  (define (escape nesting-depth docl-state . a)
    (string-join
      (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e) nesting-depth) e)) a)
      (string-append "\n" (string-multiply " " (* 2 nesting-depth))))))