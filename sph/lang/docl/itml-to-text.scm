;translates itml to a source similar indent-tree but with expressions evaluated

(library (sph lang docl itml-to-text)
  (export
    docl-itml-parsed->text
    docl-itml-port->text
    docl-itml-string->text
    docl-itml-text-env
    itml-parsed->text)
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
    (sph string))

  (define docl-itml-text-env
    (apply environment (q (sph lang docl env itml-to-text))
      (ql guile) docl-default-env-module-names))

  (module-ref docl-itml-text-env (q scm))
  (define (ascend-handle-line a nesting-depth docl-state env) a)

  (define (ascend-handle-association a nesting-depth docl-state env)
    (pair (let (prefix (first a)) (if (string? prefix) (string-append prefix ": ") prefix))
      (tail a)))

  (define-as ascend-prefix->handler-ht symbol-hashtable
    line ascend-handle-line
    inline-expr itml-eval-ascend-inline-expr
    line-expr itml-eval-ascend-line-expr
    indent-expr itml-eval-ascend-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht symbol-hashtable
    inline-scm-expr itml-eval-descend-inline-scm-expr
    line-scm-expr itml-eval-descend-line-scm-expr
    indent-scm-expr itml-eval-descend-indent-scm-expr
    indent-descend-expr itml-eval-descend-indent-descend-expr)

  (define-syntax-rule (expr->text prefix->handler a proc-arguments ...)
    (let (p (hashtable-ref prefix->handler (first a))) (and p (p (tail a) proc-arguments ...))))

  (define (ascend-expr->text a nesting-depth docl-state env)
    (or (expr->text ascend-prefix->handler-ht a nesting-depth docl-state env) a))

  (define (descend-expr->text a re-descend nesting-depth docl-state env)
    (pass-if (expr->text descend-prefix->handler-ht a re-descend nesting-depth docl-state env)
      any->string))

  (define (handle-top-level-terminal a nesting-depth docl-state env)
    (if (eqv? (q line-empty) a) "" a))

  (define (handle-terminal a . states) (pair (apply handle-top-level-terminal a states) states))

  (define itml-parsed->text
    (itml-parsed->result-proc
      (l (a nesting-depth docl-state env) (prefix-tree->indent-tree-string a nesting-depth))
      (itml-descend-proc descend-expr->text) (itml-ascend-proc ascend-expr->text)
      handle-top-level-terminal handle-terminal))

  (define docl-itml-parsed->text
    (docl-itml-parsed->result-proc itml-parsed->text docl-itml-text-env))

  (define docl-itml-port->text (docl-itml-port->result-proc itml-parsed->text docl-itml-text-env))
  (define docl-itml-string->text (docl-itml-string->result-proc docl-itml-port->text)))
