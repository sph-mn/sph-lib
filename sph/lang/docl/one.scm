(library (sph lang docl one)
  (export
    call-with-nesting-level)
  (import)

  (define (call-with-nesting-level level c) (docl-env-set! (q nesting-level) level)
    (let (r (c)) (docl-env-set! (q nesting-level) #f) r))

  (define (adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    parsed-itml is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (define (ascend-proc env level-init create-result) "environment integer -> procedure"
    (l (e level) "list integerv -> any"
      (list
        (let ((prefix (first e)) (content (tail e)))
          (create-result prefix content e env (adjust-level level) level-init))
        (- level 1))))

  (define (descend-proc env level-init create-result)
    (l (a re-descend level)
      (let (r (create-result a re-descend level env))
        (if r (list r #f level) (list #f #t (+ 1 level))))))

  (define (descend-eval-line-expr a content level env)
    (call-with-nesting-level level
      (thunk
        (eval
          (string->datum
            (string-append
              (let (content (tail a)) (apply string-append "(" (first content) " " (tail content)))
              ")")
            read)
          env))))

  (define (descend-eval-descend-indent-expr a content level env)
    (call-with-nesting-level level
      (thunk
        (let* ((content (tail a)) (prefix (first content)))
          (if (string-equal? "#" prefix) (prefix-tree->indent-tree-string (tail content) level)
            ((module-ref env (string->symbol prefix)) (tail content) level))))))

  (define (descend-eval-line-scm-expr a content level env)
    (call-for-eval level
      (thunk
        (eval
          (string->datum
            (string-append
              (let (content (tail a)) (apply string-append "(" (first content) " " (tail content)))
              ")")
            read)
          env))))

  (define (descend-eval-inline-scm-expr a content level env)
    (call-for-eval level (thunk (eval (string->datum (first (tail a)) read) env))))

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

  (define (docl-itml-port->result-proc create-result env)
    (l* (input #:optional bindings keep-prev-bindings (env env) (level-init 0))
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

  (define (docl-itml-parsed->result-proc parsed-itml->result env)
    (* (input #:optional bindings keep-prev-bindings (env env) (level-init 0))
      "list [symbol-hashtable/boolean boolean environment integer] -> text
      this can also be used to convert list trees with strings to html"
      (docl-translate-any input
        (l (input) (parsed-itml->result input env (or (docl-env-ref (q nesting-level)) level-init)))
        bindings keep-prev-bindings)))

  (define (parsed-itml->result-proc descend-proc ascend-proc create-result handle-line)
    (l* (a env #:optional (level-init 0))
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
        level-init))))
