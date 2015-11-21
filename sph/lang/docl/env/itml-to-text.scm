(library (sph lang docl env itml-to-text)
  (export
    escape
    scm
    text-reverse)
  (import
    (rnrs base)
    (sph)
    (sph lang indent-syntax)
    (only (guile)
      string-join
      reverse
      negate)
    (only (sph list) map-selected)
    (only (sph string) any->string))

  (define (scm a nesting-depth docl-state)
    (if (null? a) a (if (list? (first a)) (first a) (string-join (map any->string a) " "))))

  (define (text-reverse a nesting-depth docl-state) (string-join (reverse a) " "))

  (define (escape a nesting-depth docl-state)
    (string-join
      (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e) nesting-depth) e)) a) "\n")))
