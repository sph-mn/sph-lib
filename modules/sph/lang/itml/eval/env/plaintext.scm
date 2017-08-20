(library (sph lang itml eval env plaintext)
  (export
    escape
    itml-param-insert
    scm-eval
    scm-qq)
  (import
    (guile)
    (sph)
    (sph lang indent-syntax)
    (sph list)
    (sph string))

  (define-syntax-rule (scm-qq state a ...) (any->string (qq (a ...))))
  (define-syntax-rule (scm-eval state a ...) (any->string (begin a ...)))

  (define-syntax-rule (itml-param-insert state key ...)
    (any->string (or (itml-param-ref state key ...) (quote (key ...)))))

  (define (escape state . a)
    (list-bind state (sources depth)
      (string-join
        (map (l (a) (if (list? a) (prefix-tree->indent-tree (list a) depth) a)) a)
        (string-append "\n" (string-multiply " " (* 2 depth)))))))
