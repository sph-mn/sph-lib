(library (sph lang docl env itml-to-shtml)
  (export
    docl-eval
    docl-eval-qq
    docl-param-insert
    escape)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph string)
    (only (sph lang indent-syntax) prefix-tree->indent-tree-string))

  (define-syntax-rule (docl-eval-qq nesting-depth docl-state a ...) (qq (a ...)))
  (define-syntax-rule (docl-eval nesting-depth docl-state a ...) (begin a ...))

  (define-syntax-rule (docl-param-insert nesting-depth docl-state keys ...)
    (or (alists-q-ref (tail docl-state) keys ...)
      (string-join (map any->string (list (quote keys) ...)) " ")))

  (define (escape nesting-depth docl-state . a)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n"))))
