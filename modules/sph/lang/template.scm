(library (sph lang template)
  (export
    sph-lang-template-description
    template-bindings-proc
    template-compose
    template-datum->template-proc
    template-fold
    template-get)
  (import
    (guile)
    (rnrs eval)
    (sph)
    (only (sph alist) alist-ref)
    (only (sph io read-write) rw-file->list rw-port->list)
    (only (sph list) fold-multiple))

  (define sph-lang-template-description
    "s-expression quasiquote template processor
     a template engine using implicitly quasiquoted s-expressions.
     supports composition and concatenation. creates template procedures. source data can be given as files, ports or datums.
     alternative name: s-template.
     # data structures
     template-procedure :: procedure:ref:{key default -> any:template-variable-value} any:content -> any
     template-source: single-element/(concatenated-element/(composed-element ...) ...)
     template-source: element/(element/(element ...) ...)
     element: string:path/procedure:template-procedure/port")

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
     evaluated templates from source are passed as content to the templates before them like function composition"
    (l (ref content) (fold-right (l (a prev) ((template-get env a) ref prev)) content source)))

  (define (template-bindings-proc data)
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

  (define (template-fold fold-proc bindings env source . custom-values)
    "procedure:{procedure any -> any} list environment template-source any -> any
     template-fold processes multiple template sources with or without template composition depending on the nesting of sources in the \"source\" specification.
     example call:
     (template-fold
       (l (template-result . custom-values) (pair template-result custom-values))
       (list file.sxml appended-file.sxml (list layout.sxml included-in-layout.sxml))
       (alist (q a) 1)
       (environment (rnrs base))
       (list))"
    (apply template-source-fold source
      (let (bindings-accessor (template-bindings-proc bindings))
        (l (template . custom-values)
          (apply fold-proc (template bindings-accessor #f) custom-values)))
      env custom-values)))
