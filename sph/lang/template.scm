(library (sph lang template)
  (export
    template-fold)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph alist)
    (sph read-write))

  (define (datum->template-proc a env)
    (eval (quasiquote (lambda (ref content) (unquote (list (q quasiquote) a)))) env))

  (define (template-source-get a env)
    (if (procedure? a) a
      (datum->template-proc
        (if (string? a) (rw-file->list read a) (if (port? a) (rw-port->list read a) a)) env)))

  (define (template-source-fold a fold-proc result env)
    "template-source procedure:template-fold-proc any environment -> any"
    (if (list? a)
      (fold
        (l (wrapped result)
          (fold-proc
            (if (list? wrapped)
              (l (ref content)
                (fold-right (l (e prev) ((template-source-get e env) ref prev)) #f wrapped))
              (template-source-get wrapped env))
            result))
        result a)
      (fold-proc (template-source-get a env) result)))

  (define-syntax-rule (bindings-accessor-proc data)
    ;this procedure hides the data object and the underlying data type
    (l (key . default) (alist-ref data key (if (null? default) "" (first default)))))

  (define (template-fold fold-proc source bindings env result)
    "template-fold-proc template-source alist-quoted environment any -> any
    template-source: string:path/template-procedure/port/(string:path:merged/template-procedure/port/(string:path:wrapped/template-procedure/port/list:already-ready-result ...) ...)
    template-fold-proc: procedure:{procedure:{key [default: \"\"] -> value/default} any:inserted-content}"
    (template-source-fold source
      (let (bindings-accessor (bindings-accessor-proc bindings))
        (l (e result) (fold-proc (e bindings-accessor #f) result)))
      result env))

  (define (template-apply a bindings) (a (bindings-accessor-proc bindings) #f)))