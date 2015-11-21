;translates itml to a source similar indent-tree but with expressions evaluated

(library (sph lang docl itml-to-text)
  (export
    docl-itml-env-text
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
    (sph string)
    (only (sph tree) flatten))

  (define docl-itml-env-text
    (apply environment (q (sph lang docl env itml-to-text)) docl-default-env-module-names))

  (define (ascend-handle-line a nesting-depth docl-state env) (string-join a ""))
  (define (descend-handle-double-backslash a nesting-depth docl-state env) "\\")

  (define (ascend-handle-association a nesting-depth docl-state env)
    (string-append (first a) ": " (string-join (tail a) " ")))

  (define-as ascend-prefix->handler-ht symbol-hashtable
    line ascend-handle-line
    inline-expr itml-eval-ascend-inline-expr
    line-expr itml-eval-ascend-line-expr
    indent-expr itml-eval-ascend-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht symbol-hashtable
    inline-scm-expr itml-eval-descend-inline-scm-expr
    line-scm-expr itml-eval-descend-line-scm-expr
    indent-scm-expr itml-eval-descend-indent-scm-expr
    indent-descend-expr itml-eval-descend-indent-descend-expr
    double-backslash descend-handle-double-backslash)

  (define-syntax-rule (expr->text prefix->handler a proc-arguments ...)
    (let (p (hashtable-ref prefix->handler (first a))) (and p (p (tail a) proc-arguments ...))))

  (define (ascend-expr->text a nesting-depth docl-state env)
    (or (expr->text ascend-prefix->handler-ht a nesting-depth docl-state env) a))

  (define (descend-expr->text a re-descend nesting-depth docl-state env)
    (expr->text descend-prefix->handler-ht a nesting-depth docl-state env))

  (define (handle-top-level-terminal a . states)
    (if (eqv? (q line-empty) a) "" a))

  (define (handle-terminal a . states) (pair (apply handle-top-level-terminal a states) states))

  (define itml-parsed->text
    (itml-parsed->result-proc
      (l (a nesting-depth docl-state env) (prefix-tree->indent-tree-string a nesting-depth))
      (itml-descend-proc descend-expr->text) (itml-ascend-proc ascend-expr->text)
      handle-top-level-terminal handle-terminal))

  (define docl-itml-parsed->text
    (docl-itml-parsed->result-proc itml-parsed->text docl-itml-env-text))

  (define docl-itml-port->text (docl-itml-port->result-proc itml-parsed->text docl-itml-env-text))
  (define docl-itml-string->text (docl-itml-string->result-proc docl-itml-port->text)))
