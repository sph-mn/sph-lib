(library (sph lang itml eval plaintext)
  (export
    itml-plaintext-eval
    itml-plaintext-eval-port
    itml-plaintext-eval-string)
  (import
    (guile)
    (sph)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph lang itml eval)
    (sph lang itml read)
    (sph list)
    (sph string))

  (define sph-lang-itml-eval-plaintext-description "evaluate inline code expressions")

  (define-as ascend-ht ht-create-symbol
    line (l (a . b) (string-join a ""))
    inline-expr itml-eval-asc-inline-expr
    line-expr itml-eval-asc-line-expr
    indent-expr itml-eval-asc-indent-expr
    association
    (l (a . b)
      (let (keyword (first a))
        (string-append (if (string? keyword) keyword (string-join keyword " ")) ": "
          (string-join (tail a) " ")))))

  (define-as descend-ht ht-create-symbol
    inline-scm-expr itml-eval-desc-inline-scm-expr
    line-scm-expr itml-eval-desc-line-scm-expr
    indent-scm-expr itml-eval-desc-indent-scm-expr
    indent-desc-expr itml-eval-desc-indent-expr double-backslash (l a "\\"))

  (define itml-plaintext-eval
    (let (eval (itml-eval* descend-ht ascend-ht (l (a . b) (if (eq? (q line-empty) a) "" a))))
      (l (a itml-state) "list list -> sxml"
        (list-bind itml-state (sources depth . b)
          (prefix-tree->indent-tree-string (eval a itml-state) depth)))))

  (define (itml-plaintext-eval-port a . b) (apply itml-plaintext-eval (port->itml-parsed a) b))
  (define (itml-plaintext-eval-string a . b) (apply itml-plaintext-eval (string->itml-parsed a) b)))
