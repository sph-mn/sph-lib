(library (sph lang docl itml-to-plaintext)
  (export
    docl-itml-env-plaintext
    docl-itml-env-plaintext-module-names
    docl-itml-parsed->plaintext
    docl-itml-plaintext-env
    docl-itml-port->plaintext
    docl-itml-string->plaintext
    itml-parsed->plaintext
    sph-lang-docl-itml-to-plaintext-description)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph conditional)
    (sph hashtable)
    (sph lang docl)
    (sph lang docl itml)
    (sph lang indent-syntax)
    (sph lang itml)
    (sph string))

  (define sph-lang-docl-itml-to-plaintext-description
    "translates itml to an indent-tree plaintext string with evaluated expressions")

  (define docl-itml-env-plaintext-module-names
    (pair (q (sph lang docl env itml-to-plaintext)) docl-default-env-module-names))

  (define docl-itml-env-plaintext (apply environment docl-itml-env-plaintext-module-names))
  (define (ascend-handle-line a nesting-depth docl-state env) (string-join a ""))
  (define (descend-handle-double-backslash a nesting-depth docl-state env) "\\")

  (define (ascend-handle-association a nesting-depth docl-state env)
    (string-append (first a) ": " (string-join (tail a) " ")))

  (define-as ascend-prefix->handler-ht ht-create-symbol
    line ascend-handle-line
    inline-expr itml-eval-ascend-inline-expr
    line-expr itml-eval-ascend-line-expr
    indent-expr itml-eval-ascend-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht ht-create-symbol
    inline-scm-expr itml-eval-descend-inline-scm-expr
    line-scm-expr itml-eval-descend-line-scm-expr
    indent-scm-expr itml-eval-descend-indent-scm-expr
    indent-descend-expr itml-eval-descend-indent-expr
    double-backslash descend-handle-double-backslash)

  (define-syntax-rule (expr->plaintext prefix->handler a proc-arguments ...)
    (let (handler (ht-ref prefix->handler (first a)))
      (and handler (handler (tail a) proc-arguments ...))))

  (define (ascend-expr->plaintext a nesting-depth docl-state env)
    (or (expr->plaintext ascend-prefix->handler-ht a nesting-depth docl-state env) a))

  (define (descend-expr->plaintext a re-descend nesting-depth docl-state env)
    (expr->plaintext descend-prefix->handler-ht a re-descend nesting-depth docl-state env))

  (define (handle-top-level-terminal a . states) (if (eqv? (q line-empty) a) "" a))
  (define (handle-terminal a . states) (pair (apply handle-top-level-terminal a states) states))

  (define itml-parsed->plaintext
    (itml-parsed->result-proc
      (l (a nesting-depth docl-state env) (prefix-tree->indent-tree-string a nesting-depth))
      (itml-descend-proc descend-expr->plaintext)
      (itml-ascend-proc ascend-expr->plaintext itml-adjust-nesting-depth) handle-top-level-terminal
      handle-terminal))

  (define docl-itml-parsed->plaintext (docl-itml-parsed->result-proc itml-parsed->plaintext))
  (define docl-itml-port->plaintext (docl-itml-port->result-proc itml-parsed->plaintext))
  (define docl-itml-string->plaintext (docl-itml-string->result-proc docl-itml-port->plaintext)))
