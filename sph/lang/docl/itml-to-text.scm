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
    (sph lang docl)
    (sph lang docl itml)
    (sph lang indent-syntax)
    (sph string))

  (define docl-itml-text-env
    (apply environment (q (sph lang docl env itml-to-text)) docl-default-env-module-names))

  (define (ascend-handle-line a nesting-depth docl-state env) a)

  (define (ascend-handle-association a nesting-depth docl-state env)
    (pair (let (prefix (first a)) (if (string? prefix) (string-append prefix ": ") prefix))
      (tail a)))

  (define (ascend-expr->text a nesting-depth docl-state env)
    (let (a-tail (tail a))
      (case (first a) ((line) (ascend-handle-line a-tail nesting-depth docl-state env))
        ((inline-expr) (ascend-eval-inline-expr a-tail nesting-depth docl-state env))
        ((line-expr) (ascend-eval-line-expr a-tail nesting-depth docl-state env))
        ((indent-expr) (ascend-eval-indent-expr a-tail nesting-depth docl-state env))
        ((association) (ascend-handle-association a-tail nesting-depth docl-state env)) (else a))))

  (define (descend-expr->text a re-descend nesting-depth docl-state env)
    (pass-if
      (let (a-tail (tail a))
        (case (first a)
          ((inline-scm-expr) (descend-eval-inline-scm-expr a-tail nesting-depth docl-state env))
          ( (indent-descend-expr)
            (descend-eval-indent-descend-expr a-tail nesting-depth docl-state env))
          ((line-scm-expr) (descend-eval-line-scm-expr a-tail nesting-depth docl-state env))
          ((indent-scm-expr) (descend-eval-indent-scm-expr a-tail nesting-depth docl-state env))
          (else #f)))
      any->string))

  (define (handle-top-level-terminal a nesting-depth docl-state env)
    (if (eqv? (q line-empty) a) "" a))

  (define (handle-terminal a . states) (pair (apply handle-top-level-terminal a states) states))

  (define itml-parsed->text
    (itml-parsed->result-proc
      (l (a nesting-depth docl-state env) (prefix-tree->indent-tree-string a nesting-depth))
      (descend-proc descend-expr->text) (ascend-proc ascend-expr->text)
      handle-top-level-terminal handle-terminal))

  (define docl-itml-parsed->text
    (docl-itml-parsed->result-proc itml-parsed->text docl-itml-text-env))

  (define docl-itml-port->text (docl-itml-port->result-proc itml-parsed->text docl-itml-text-env))
  (define docl-itml-string->text (docl-itml-string->result-proc docl-itml-port->text)))
