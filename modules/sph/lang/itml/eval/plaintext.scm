(library (sph lang itml eval plaintext)
  (export)
  (import
    (guile)
    (sph)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph lang itml eval)
    (sph lang itml read)
    (sph string))

  (define sph-lang-itml-eval-plaintext-description
    "evaluate inline code expressions and translate itml to plaintext")

  (define (ascend-handle-line a depth state env) (string-join a ""))
  (define (descend-handle-double-backslash a depth state env) "\\")

  (define (ascend-handle-association a depth state env)
    (string-append (first a) ": " (string-join (tail a) " ")))

  (define-as ascend-prefix->handler-ht ht-create-symbol
    line ascend-handle-line
    inline-expr itml-eval-asc-inline-expr
    line-expr itml-eval-asc-line-expr
    indent-expr itml-eval-asc-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht ht-create-symbol
    inline-scm-expr itml-eval-desc-inline-scm-expr
    line-scm-expr itml-eval-desc-line-scm-expr
    indent-scm-expr itml-eval-desc-indent-scm-expr
    indent-desc-expr itml-eval-desc-indent-expr double-backslash descend-handle-double-backslash)

  (define-syntax-rule (expr->plaintext prefix->handler a proc-arguments ...)
    (let (handler (ht-ref prefix->handler (first a)))
      (and handler (handler (tail a) proc-arguments ...))))

  (define (ascend-expr->plaintext a depth state env)
    (or (expr->plaintext ascend-prefix->handler-ht a depth state env) a))

  (define (descend-expr->plaintext a re-descend depth state env)
    (expr->plaintext descend-prefix->handler-ht a re-descend depth state env))

  (define (handle-top-level-terminal a . states) (if (eqv? (q line-empty) a) "" a))
  (define (handle-terminal a . states) (pair (apply handle-top-level-terminal a states) states))

  (define itml-parsed->plaintext
    (itml-parsed->result-proc (l (a depth state env) (prefix-tree->indent-tree-string a depth))
      (itml-descend-proc descend-expr->plaintext)
      (itml-ascend-proc ascend-expr->plaintext itml-adjust-depth) handle-top-level-terminal
      handle-terminal))

  (define docl-itml-parsed->plaintext (docl-itml-parsed->result-proc itml-parsed->plaintext))
  (define docl-itml-port->plaintext (docl-itml-port->result-proc itml-parsed->plaintext))
  (define docl-itml-string->plaintext (docl-itml-string->result-proc docl-itml-port->plaintext)))
