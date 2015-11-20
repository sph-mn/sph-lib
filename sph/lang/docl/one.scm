(library (sph lang docl one)
  (export
    adjust-level
    ascend-eval-indent-expr
    ascend-eval-inline-expr
    ascend-eval-line-expr
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
    itml-parsed->result-proc)
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

  (define (adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    parsed-itml is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (begin
    (define (descend-eval-line-expr a nesting-level docl-state env)
      (eval
        (string->datum (string-append (apply string-append "(" (first a) " " (tail a)) ")") read) env))
    (define (descend-eval-indent-descend-expr a nesting-level docl-state env)
      (thunk ((module-ref env (string->symbol (first a))) (tail a))))
    (define (descend-eval-indent-scm-expr a nesting-level docl-state env)
      (eval
        (string->datum
          (call-with-output-string
            (l (port)
              ;display converts a string-list to a scheme-expression with symbols. flatten is used to ignore the indent-tree nesting
              (display (flatten a) port))))
        env))
    (define (descend-eval-line-scm-expr a nesting-level docl-state env)
      ;eval is used so that syntax forms work in docl expressions.
      (eval
        (string->datum (string-append (apply string-append "(" (first a) " " (tail a)) ")") read) env))
    (define (descend-eval-inline-scm-expr a nesting-level docl-state env)
      (eval (string->datum (first a) read) env))
    (define (ascend-eval-inline-expr a nesting-level docl-state env)
      (eval (list (string->symbol (first a)) (list (q quote) (tail a)) nesting-level docl-state)
        env))
    (define (ascend-eval-line-expr a nesting-level docl-state env)
      (eval (pair (string->symbol (first a)) (tail a)) env))
    (define (ascend-eval-indent-expr a nesting-level docl-state env)
      ((module-ref env (string->symbol (first a))) (tail a))))

  (define (ascend-proc-proc create-result) "environment integer -> procedure"
    (l (env)
      (l (e nesting-level docl-state env) "list integer vhash environment -> any"
        (list (create-result e (adjust-level nesting-level) docl-state env) (- nesting-level 1)))))

  (define (descend-proc create-result)
    (l (a re-descend nesting-level docl-state env)
      (let (r (create-result a re-descend nesting-level docl-state env))
        (if r (list r #f nesting-level) (list #f #t (+ 1 nesting-level))))))

  (define (docl-itml-port->result-proc create-result default-env)
    (l* (input #:optional (nesting-level 0) (docl-state docl-state-empty) (env default-env))
      "read itml from a port, parse it and translate it to text"
      (docl-translate-port input
        (l (input docl-state)
          (create-result (port->parsed-itml input) nesting-level docl-state env))
        docl-state)))

  (define (docl-itml-string->result-proc port->result)
    (l (input . port->result-arguments) "string _ ... -> text"
      (apply port->result (open-input-string input) port->result-arguments)))

  (define (docl-itml-parsed->result-proc parsed-itml->result default-env)
    (l* (input #:optional (nesting-level 0) (docl-state docl-state-empty) (env default-env))
      "list [integer vhash environment] -> any
      this can also be used to convert list trees with strings to html"
      (docl-translate-any input
        (l (input docl-state) (parsed-itml->result input nesting-level docl-state env)) docl-state)))

  (define
    (itml-parsed->result-proc descend ascend create-result handle-top-level-terminal
      handle-terminal)
    (l* (a #:optional (nesting-level 0) (docl-state docl-state-empty) (env default-env))
      "list [integer vhash environment] -> any
      a translator for parsed-itml"
      (create-result
        (map
          (l (e)
            (if (list? e)
              (first
                (tree-transform-with-state e descend
                  ascend handle-terminal nesting-level docl-state env))
              (handle-top-level-terminal e nesting-level docl-state env)))
          a)
        nesting-level docl-state env))))
