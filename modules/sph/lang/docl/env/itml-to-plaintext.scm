(library (sph lang docl env itml-to-plaintext)
  (export
    docl-eval
    docl-eval-qq
    docl-param-insert
    escape)
  (import
    (sph)
    (sph lang indent-syntax)
    (only (guile)
      string-join
      reverse
      negate)
    (only (sph list) map-selected)
    (only (sph string) any->string string-multiply))

  (define-syntax-rule (docl-eval-qq nesting-depth docl-state a ...) (any->string (qq (a ...))))
  (define-syntax-rule (docl-eval nesting-depth docl-state a ...) (any->string (begin a ...)))

  (define-syntax-rule (docl-param-insert nesting-depth docl-state keys ...)
    (any->string (or (alists-q-ref (tail docl-state) keys ...) (list (quote keys) ...))))

  (define (escape nesting-depth docl-state . a)
    (string-join
      (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e) nesting-depth) e)) a)
      (string-append "\n" (string-multiply " " (* 2 nesting-depth))))))
