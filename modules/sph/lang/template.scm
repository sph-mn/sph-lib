(library (sph lang template)
  (export
    template-bindings-proc
    template-compose
    template-datum->template-proc
    template-fold
    template-get)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (only (sph alist) alist-ref)
    (only (sph list) fold-multiple)
    (only (sph read-write) rw-file->list rw-port->list))

  ;alternative name: s-template.
  ;a template engine using implicitly quasiquoted s-expressions.
  ;creates template procedures from source data read from files or ports

  (define (template-datum->template-proc a env)
    "any:scheme-datum environment -> procedure:template-proc
    creates a template-proc from an unevaluated template scheme datum"
    (eval (quasiquote (lambda (ref content) (unquote (list (q quasiquote) a)))) env))

  (define (template-get env a) "environment procedure/string/port/any -> procedure"
    (if (procedure? a) a
      (template-datum->template-proc
        (if (string? a) (rw-file->list read a) (if (port? a) (rw-port->list read a) a)) env)))

  (define (template-compose env . source)
    "environment template-source ... -> procedure:template-proc
    evaluated templates from source are passed as content to the templates before them, like function composition"
    (l (ref content) (fold-right (l (e prev) ((template-get env e) ref prev)) content source)))

  (define (template-bindings-proc data)
    ;this procedure abstracts the data object and its data type
    (case-lambda (() data)
      ((key . default) (alist-ref data key (if (null? default) "" (first default))))))

  (define (template-source-fold a fold-proc env . result)
    "template-source procedure:template-fold-proc any environment -> any"
    (if (list? a)
      (apply fold-multiple
        (l (wrapped . result)
          (apply fold-proc
            (if (list? wrapped) (apply template-compose env wrapped) (template-get env wrapped))
            result))
        a result)
      (apply fold-proc (template-get env a) result)))

  (define (template-fold fold-proc bindings env source . result)
    "procedure:{procedure any -> any} list environment template-source any -> any
    template-fold processes multiple template sources with or without template composition depending on the nesting of sources in the \"source\" specification.

    element: string:path/template-procedure/port
    template-source: element/(element/(element ...) ...)
    template-source: single-element/(multiple-element/(composed-element ...) ...)
    example
    (template-fold (l (template-result result) (pair template-result result)) source bindings env (list))"
    (apply template-source-fold source
      (let (bindings-accessor (template-bindings-proc bindings))
        (l (template . result) (apply fold-proc (template bindings-accessor #f) result)))
      env result)))
