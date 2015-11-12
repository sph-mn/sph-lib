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
    (sph lang docl)
    (sph lang docl env)
    (sph lang indent-syntax)
    (sph lang parser itml)
    (sph read-write)
    (only (sph list) insert-second)
    (only (sph one) string->datum first-as-result)
    (only (sph string) any->string string-equal?)
    (only (sph tree) flatten tree-transform-with-state))

  ;translates indent-tree-markup-language to indent-tree text by evaluating all expressions.
  (define docl-itml-text-env (apply environment docl-default-env-module-names))

  (define (call-for-eval level c) (docl-env-set! (q nesting-level) level)
    (let (r (c)) (docl-env-set! (q nesting-level) #f) r))

  (define-syntax-rule (ascend-expr->text prefix content e env level level-init)
    (case prefix ((line) content)
      ;eval is used so that syntax forms work in docl expressions.
      ( (inline-expr)
        (call-for-eval level
          (thunk (eval (list (string->symbol (first content)) (list (q quote) (tail content))) env))))
      ( (line-expr)
        (call-for-eval level
          (thunk (eval (pair (string->symbol (first content)) (tail content)) env))))
      ( (indent-expr)
        (call-for-eval level
          (thunk ((module-ref env (string->symbol (first content))) (tail content)))))
      ( (association)
        (pair (let (e (first content)) (if (string? e) (string-append e ": ") e)) (tail content)))
      (else e)))

  (define (adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    parsed-itml is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (define (ascend-proc env level-init)
    (l (e level)
      (list
        (let ((prefix (first e)) (content (tail e)))
          (ascend-expr->text prefix content e env (adjust-level level) level-init))
        (- level 1))))

  (define (descend-expr->text a re-descend level env)
    (case (first a)
      ( (inline-scm-expr)
        (any->string (call-for-eval level (thunk (eval (string->datum (first (tail a)) read) env)))))
      ( (indent-descend-expr)
        (any->string
          (call-for-eval level
            (thunk
              (let* ((content (tail a)) (prefix (first content)))
                (if (string-equal? "#" prefix)
                  (prefix-tree->indent-tree-string (tail content) level)
                  ((module-ref env (string->symbol prefix)) (tail content) level)))))))
      ( (line-scm-expr)
        (any->string
          (call-for-eval level
            (thunk
              (eval
                (string->datum
                  (string-append
                    (let (content (tail a))
                      (apply string-append "(" (first content) " " (tail content)))
                    ")")
                  read)
                env)))))
      ( (indent-scm-expr)
        (any->string
          (call-for-eval level
            (thunk
              (eval
                (string->datum
                  (call-with-output-string
                    (l (port)
                      ;display converts a string-list to a scheme-expression with symbols. flatten is used to ignore the indent-tree nesting
                      (display (flatten (tail a)) port))))
                env)))))
      (else #f)))

  (define (descend-proc env level-init)
    (l (a re-descend level)
      (let (r (descend-expr->text a re-descend level env))
        (if r (list r #f level) (list #f #t (+ 1 level))))))

  (define* (parsed-itml->text a env #:optional (level-init 0))
    "list environment [integer] -> string
    a translator for parsed-itml. does not depend on docl"
    (prefix-tree->indent-tree-string
      (map
        (l (e)
          (if (list? e)
            (first
              (tree-transform-with-state e (descend-proc env level-init)
                (ascend-proc env level-init) (l a a) level-init))
            (if (eqv? (q line) e) "" e)))
        a)
      level-init))

  (define*
    (docl-itml-parsed->text input #:optional bindings keep-prev-bindings (env docl-itml-text-env)
      (level-init 0))
    "list [hashtable-quoted/boolean boolean environment integer] -> text
    this can also be used to convert list trees with strings to html"
    (docl-translate-any input
      (l (input) (parsed-itml->text input env (or (docl-env-ref (q nesting-level)) level-init)))
      bindings keep-prev-bindings))

  (define*
    (docl-itml-port->text input #:optional bindings keep-prev-bindings (env docl-itml-text-env)
      (level-init 0))
    "port [hashtable-quoted/boolean boolean environment integer] -> text
    read itml from a port, parse it and translate it to text"
    (docl-translate-port input
      (l (input)
        (parsed-itml->text (port->parsed-itml input) env
          (or (docl-env-ref (q nesting-level)) level-init)))
      bindings keep-prev-bindings))

  (define (docl-itml-string->text input . docl-itml-port->text-arguments)
    "string [hashtable-quoted/boolean boolean environment integer] -> text"
    (apply docl-itml-port->text (open-input-string input) docl-itml-port->text-arguments)))
