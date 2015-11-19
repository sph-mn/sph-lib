;translates itml to a source similar indent-tree but with expressions evaluated

(library (sph lang docl itml-to-text)
  (export
    docl-itml-parsed->text
    docl-itml-port->text
    docl-itml-string->text
    docl-itml-text-env
    parsed-itml->text)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph conditional)
    (sph lang docl)
    (sph lang docl one)
    (sph lang indent-syntax)
    (sph string))

  (define docl-itml-text-env (apply environment docl-default-env-module-names))

  (define (ascend-expr->text prefix content e env level level-init)
    (case prefix ((line) (ascend-handle-line e content level env))
      ((inline-expr) (ascend-eval-inline-expr e content level env))
      ((line-expr) (ascend-eval-line-expr e content level env))
      ((indent-expr) (ascend-eval-indent-expr e content level env))
      ((association) (ascend-handle-association e content level env)) (else e)))

  (define (descend-expr->text a re-descend level env)
    (pass-if
      (case (first a) ((inline-scm-expr) (descend-eval-inline-scm-expr (tail a) level env))
        ((indent-descend-expr) (descend-eval-indent-descend-expr (tail a) level env))
        ((line-scm-expr) (descend-eval-line-scm-expr (tail a) level env))
        ((indent-scm-expr) (descend-eval-indent-scm-expr (tail a) level env)) (else #f))
      any->string))

  (define parsed-itml->text
    (parsed-itml->result-proc (descend-proc-proc descend-expr->text)
      (ascend-proc-proc ascend-expr->text) prefix-tree->indent-tree-string (const "")))

  (define docl-itml-parsed->text
    (docl-itml-parsed->result-proc parsed-itml->text docl-itml-text-env))

  (define docl-itml-port->text (docl-itml-port->result-proc parsed-itml->text docl-itml-text-env))
  (define docl-itml-string->text (docl-itml-string->result-proc docl-itml-port->text)))
