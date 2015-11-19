(library (sph lang docl one)
  (export
    adjust-level
    ascend-eval-indent-expr
    ascend-eval-inline-expr
    ascend-eval-line-expr
    ascend-handle-association
    ascend-handle-line
    ascend-proc-proc
    call-for-eval
    descend-eval-indent-descend-expr
    descend-eval-indent-scm-expr
    descend-eval-inline-expr
    descend-eval-inline-scm-expr
    descend-eval-line-expr
    descend-eval-line-scm-expr
    descend-proc-proc
    docl-itml-parsed->result-proc
    docl-itml-port->result-proc
    docl-itml-string->result-proc
    parsed-itml->result-proc)
  (import
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph conditional)
    (sph lang docl)
    (sph lang docl env)
    (sph lang parser itml)
    (only (guile)
      call-with-output-string
      display
      module-ref
      read
      open-input-string)
    (only (sph lang indent-syntax) prefix-tree->indent-tree-string)
    (only (sph one) string->datum)
    (only (sph string) string-equal?)
    (only (sph tree) flatten tree-transform-with-state))

  (define (call-for-eval level c) (docl-env-set! (q nesting-level) level)
    (let (r (c)) (docl-env-set! (q nesting-level) #f) r))

  (define (adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    parsed-itml is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (define (ascend-proc-proc create-result) "environment integer -> procedure"
    (l (env level-init)
      (l (e level) "list integerv -> any"
        (list
          (let ((prefix (first e)) (content (tail e)))
            (create-result prefix content e env (adjust-level level) level-init))
          (- level 1)))))

  (define (descend-proc-proc create-result)
    (l (env level-init)
      (l (a re-descend level)
        (let (r (create-result a re-descend level env))
          (if r (list r #f level) (list #f #t (+ 1 level)))))))

  (define (descend-eval-line-expr content level env)
    (call-for-eval level
      (thunk
        (eval
          (string->datum
            (string-append (apply string-append "(" (first content) " " (tail content)) ")") read)
          env))))

  (define (descend-eval-indent-descend-expr content level env)
    (call-for-eval level
      (thunk
        (let (prefix (first content))
          (if (string-equal? "#" prefix) (prefix-tree->indent-tree-string (tail content) level)
            ((module-ref env (string->symbol prefix)) (tail content) level))))))

  (define (descend-eval-indent-scm-expr content level env)
    (call-for-eval level
      (thunk
        (eval
          (string->datum
            (call-with-output-string
              (l (port)
                ;display converts a string-list to a scheme-expression with symbols. flatten is used to ignore the indent-tree nesting
                (display (flatten content) port))))
          env))))

  (define (descend-eval-line-scm-expr content level env)
    ;eval is used so that syntax forms work in docl expressions.
    (call-for-eval level
      (thunk
        (eval
          (string->datum
            (string-append (apply string-append "(" (first content) " " (tail content)) ")") read)
          env))))

  (define (descend-eval-inline-scm-expr content level env)
    (call-for-eval level (thunk (eval (string->datum (first content) read) env))))

  (define (ascend-eval-inline-expr a content level env)
    (call-for-eval level
      (thunk (eval (list (string->symbol (first content)) (list (q quote) (tail content))) env))))

  (define (ascend-eval-line-expr a content level env)
    (call-for-eval level (thunk (eval (pair (string->symbol (first content)) (tail content)) env))))

  (define (ascend-eval-indent-expr a content level env)
    (call-for-eval level (thunk ((module-ref env (string->symbol (first content))) (tail content)))))

  (define (ascend-handle-association a content level env)
    (pair (let (e (first content)) (if (string? e) (string-append e ": ") e)) (tail content)))

  (define (ascend-handle-line a content level env) content)

  (define (docl-itml-port->result-proc create-result default-env)
    (l* (input #:optional bindings keep-prev-bindings (env default-env) (level-init 0))
      "port [symbol-hashtable/boolean boolean environment integer] -> text
      read itml from a port, parse it and translate it to text"
      (docl-translate-port input
        (l (input)
          (create-result (port->parsed-itml input) env
            (or (docl-env-ref (q nesting-level)) level-init)))
        bindings keep-prev-bindings)))

  (define (docl-itml-string->result-proc port->result)
    (l (input . port->result-arguments)
      "string [symbol-hashtable/boolean boolean environment integer] -> text"
      (apply port->result (open-input-string input) port->result-arguments)))

  (define (docl-itml-parsed->result-proc parsed-itml->result default-env)
    (l* (input #:optional bindings keep-prev-bindings (env default-env) (level-init 0))
      "list [symbol-hashtable/boolean boolean environment integer] -> text
      this can also be used to convert list trees with strings to html"
      (docl-translate-any input
        (l (input) (parsed-itml->result input env (or (docl-env-ref (q nesting-level)) level-init)))
        bindings keep-prev-bindings)))

  (define (parsed-itml->result-proc descend-proc ascend-proc create-result create-line)
    (l* (a env #:optional (level-init 0))
      "list environment [integer] -> string
      a translator for parsed-itml. does not depend on docl"
      (create-result
        (map
          (l (e)
            (if (list? e)
              (first
                (tree-transform-with-state e (descend-proc env level-init)
                  (ascend-proc env level-init) (l a a) level-init))
              (if (eqv? (q line) e) (create-line) e)))
          a)
        level-init))))
