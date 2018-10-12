(library (sph lang itml eval plaintext)
  (export
    itml-plaintext-eval
    itml-plaintext-eval-file
    itml-plaintext-eval-port
    itml-plaintext-eval-string
    itml-plaintext-false)
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
  (define itml-plaintext-false "_")

  (define-as ascend-ht ht-create-symbol-q
    line (l (a . b) (string-join a ""))
    inline-text-expression itml-eval-asc-inline-expr
    line-text-expression itml-eval-asc-line-expr
    indent-text-expression itml-eval-asc-indent-expr
    association
    (l (a . b)
      (let (keyword (first a))
        (string-append (if (string? keyword) keyword (string-join keyword " ")) ": "
          (string-join (tail a) " ")))))

  (define (string-if-false proc)
    "when an itml expression evaluates to false, return a string instead, to
     mark the place of a failed expression in the output text. nested scm expressions are not affected"
    (l a (or (apply proc a) itml-plaintext-false)))

  (define-as descend-ht ht-create-symbol-q
    inline-scm-expression (string-if-false itml-eval-desc-inline-scm-expr)
    line-scm-expression (string-if-false itml-eval-desc-line-scm-expr)
    indent-scm-expression (string-if-false itml-eval-desc-indent-scm-expr)
    indent-descend-expression (string-if-false itml-eval-desc-indent-expr)
    double-backslash (l a "\\"))

  (define itml-plaintext-eval
    (let (eval (itml-eval* descend-ht ascend-ht #:terminal (l (a . b) (if (eq? (q line-empty) a) "" a))))
      (l (a itml-state) "list list -> sxml"
        (list-bind itml-state (sources depth . b)
          (prefix-tree->indent-tree (eval a itml-state) depth)))))

  (define (itml-plaintext-eval-port a . b) (apply itml-plaintext-eval (port->itml-parsed a) b))
  (define (itml-plaintext-eval-string a . b) (apply itml-plaintext-eval (string->itml-parsed a) b))

  (define (itml-plaintext-eval-file a . b)
    (call-with-input-file a (l (a) (apply itml-plaintext-eval-port a b)))))
