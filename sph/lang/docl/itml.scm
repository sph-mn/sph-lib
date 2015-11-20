(library (sph lang docl itml)
  (export
    adjust-level
    ascend-eval-indent-expr
    ascend-eval-inline-expr
    ascend-eval-line-expr
    ascend-proc
    descend-eval-indent-descend-expr
    descend-eval-indent-scm-expr
    descend-eval-inline-expr
    descend-eval-inline-scm-expr
    descend-eval-line-expr
    descend-eval-line-scm-expr
    descend-proc
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
    (sph lang parser itml)
    (only (guile)
      call-with-output-string
      display
      string-join
      module-ref
      read
      open-input-string)
    (only (sph lang indent-syntax) prefix-tree->indent-tree-string)
    (only (sph one) string->datum)
    (only (sph string) parenthesise)
    (only (sph tree) flatten tree-transform-with-state))

  (define (adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    itml-parsed is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (define (module-ref-string env a) (module-ref env (string->symbol a)))

  (begin
    (define (descend-eval-line-expr a nesting-depth docl-state env)
      ((module-ref-string env (first a)) (tail a)))
    (define (descend-eval-indent-descend-expr a nesting-depth docl-state env)
      ((module-ref env (string->symbol (first a))) (tail a)))
    (define (descend-eval-indent-scm-expr a nesting-depth docl-state env)
      (string->datum (parenthesise (string-join (flatten a) " ")) read))
    (define (descend-eval-line-scm-expr a nesting-depth docl-state env)
      ((module-ref-string env (first a)) (string->datum (parenthesise (tail a)) read)))
    (define (descend-eval-inline-scm-expr a nesting-depth docl-state env)
      (eval (string->datum (first a) read) env))
    (define (ascend-eval-inline-expr a nesting-depth docl-state env)
      ((module-ref-string env (first a)) (tail a) nesting-depth docl-state))
    (define (ascend-eval-line-expr a nesting-depth docl-state env)
      ((module-ref-string env (first a)) (tail a)))
    (define (ascend-eval-indent-expr a nesting-depth docl-state env)
      ((module-ref-string env (first a)) (tail a))))

  (define (ascend-proc create-result) "environment integer -> procedure"
    (l (a nesting-depth docl-state env) "list integer vhash environment -> any"
      (debug-log a)
      (list (create-result a (adjust-level nesting-depth) docl-state env) (- nesting-depth 1))))

  (define (descend-proc create-result)
    (l (a re-descend nesting-depth docl-state env) (debug-log a)
      (let (r (create-result a re-descend nesting-depth docl-state env))
        (if r (list r #f nesting-depth) (list #f #t (+ 1 nesting-depth))))))

  (define (docl-itml-port->result-proc create-result default-env)
    (l* (input #:optional (nesting-depth 0) (docl-state docl-state-empty) (env default-env))
      "read itml from a port, parse it and translate it to text"
      (docl-translate-port input
        (l (input docl-state)
          (create-result (port->itml-parsed input) nesting-depth docl-state env))
        docl-state)))

  (define (docl-itml-string->result-proc port->result)
    (l (input . port->result-arguments) "string _ ... -> text"
      (apply port->result (open-input-string input) port->result-arguments)))

  (define (docl-itml-parsed->result-proc itml-parsed->result default-env)
    (l* (input #:optional (nesting-depth 0) (docl-state docl-state-empty) (env default-env))
      "list [integer vhash environment] -> any
      this can also be used to convert list trees with strings to html"
      (docl-translate-any input
        (l (input docl-state) (itml-parsed->result input nesting-depth docl-state env)) docl-state)))

  (define
    (itml-parsed->result-proc create-result descend ascend  handle-top-level-terminal
      handle-terminal
      default-env)
    (l (a nesting-depth docl-state env)
      "list [integer vhash environment] -> any
      a translator for itml-parsed"
      (create-result
        (map
          (l (e)
            (if (list? e)
              (first
                (tree-transform-with-state e descend
                  ascend handle-terminal nesting-depth docl-state env))
              (handle-top-level-terminal e nesting-depth docl-state env)))
          a)
        nesting-depth docl-state env))))
