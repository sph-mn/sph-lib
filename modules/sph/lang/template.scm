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
    (only (sph list) fold-multiple))

  (define sph-lang-template-description
    "s-expression template processor.
     a template engine that interprets expressions as elements of a quasiquoted list.
     supports concatenation and composition. source data can be given as files, ports or datums.
     creates template procedures.
     alternative name: s-template
     # data structures
     template-procedure :: procedure:{symbol:key [default] -> variable-value} any:content -> any
     template-source: element/(element/(element ...) ...)
     template-source: single-elements/(concatenated-elements/(composed-elements ...) ...)
     element: string:path/procedure:template-procedure/port/any")

  (define (rw-file->list read path)
    (call-with-input-file path (l (port) (rw-port->list read port))))

  (define (rw-port->list read port)
    (let loop ((a (read port))) (if (eof-object? a) (list) (pair a (loop (read port))))))

  (define (template-datum->template-proc a env)
    "any:scheme-datum environment -> procedure:template-proc
     creates a template-procedure from an unevaluated template scheme datum.
     additionally to the bindings from the given environment, the bindings \"v\" and \"content\" are available in scope.
     v can be used as (v (quote variable-name)) to insert template variables. content contains the value
     eventually inserted via composition"
    (eval (quasiquote (lambda (v content) (unquote (list (q quasiquote) a)))) env))

  (define (template-get env a) "environment procedure/string/port/any -> procedure"
    (if (procedure? a) a
      (template-datum->template-proc
        (if (string? a) (rw-file->list read a) (if (port? a) (rw-port->list read a) a)) env)))

  (define (template-compose env . source)
    "environment template-source ... -> procedure:template-proc
     evaluated templates from source are passed as content to the templates before them like function composition"
    (l (v content) (fold-right (l (a prev) ((template-get env a) v prev)) content source)))

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
     template-fold processes multiple template sources. it creates the s-expression structure
     with variables replaced and passes individual expressions to fold-proc which can then process them further,
     for example translate sxml to an xml string.
     how sources are combined depends on the nesting depth they are specified with.
     first nesting level: (a b c ...)
       templates are appended
     second nesting level: (a (b c d) ...)
       templates are composed/inserted. similar to function application, template b receives the result of template c,
       which receives the result of template d. this is appended to a
     source elements can be filesystem path strings, template procedures or other scheme datums.
     if the datums to insert are lists, then they should be nested in two lists (((content ...))) to avoid the automatic
     concatenation or composition.
     example call
       (template-fold
         (l (template-result . custom-values) (pair template-result custom-values))
         (list \"file.sxml\" \"appended-file.sxml\" (list \"layout.sxml\" \"inserted-into-layout.sxml\"))
         (alist-q a 1)
         (environment (rnrs base))
         (list))"
    (apply template-source-fold source
      (let (bindings-accessor (template-bindings-proc bindings))
        (l (template . custom-values)
          (apply fold-proc (template bindings-accessor #f) custom-values)))
      env custom-values)))
