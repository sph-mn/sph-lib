;generic bindings to evaluate itml expressions a and convert from itml to another language

(library (sph lang docl itml)
  (export
    docl-itml-parsed->result-proc
    docl-itml-port->result-proc
    docl-itml-string->result-proc
    itml-adjust-level
    itml-ascend-proc
    itml-descend-proc
    itml-eval-ascend-indent-expr
    itml-eval-ascend-inline-expr
    itml-eval-ascend-line-expr
    itml-eval-descend-indent-expr
    itml-eval-descend-indent-scm-expr
    itml-eval-descend-inline-expr
    itml-eval-descend-inline-scm-expr
    itml-eval-descend-line-expr
    itml-eval-descend-line-scm-expr
    itml-parsed->result-proc)
  (import
    (ice-9 threads)
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
    (only (sph list) simplify-list)
    (only (sph one) string->datum)
    (only (sph string) parenthesise)
    (only (sph tree) flatten tree-transform-with-state))

  (define (itml-adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    itml-parsed is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (define (itml-list-eval a env . proc-arguments)
    (let
      ( (docl-call (l (proc . arguments) (apply proc arguments proc-arguments)))
        (docl-proc
          (eval
            (qq
              (l (docl-call nesting-depth docl-state)
                ((unquote (first a)) nesting-depth docl-state (unquote-splicing (tail a)))))
            env)))
      (apply docl-proc docl-call proc-arguments)))

  (define-syntax-rule (itml-list-string-eval a env proc-arguments ...)
    (itml-list-eval (pair (string->symbol (first a)) (map (l (a) (list (q quote) a)) (tail a))) env
      proc-arguments ...))

  (define (itml-eval-1 a nesting-depth docl-state env)
    (itml-list-string-eval a env nesting-depth docl-state))

  (define (itml-eval-2 a nesting-depth docl-state env)
    (let (a (simplify-list (string->datum (parenthesise (string-join (flatten a) " ")) read)))
      (itml-list-eval a env nesting-depth docl-state)))

  (define itml-eval-descend-line-scm-expr itml-eval-2)
  (define itml-eval-descend-inline-scm-expr itml-eval-2)
  (define itml-eval-descend-indent-scm-expr itml-eval-2)
  (define itml-eval-descend-line-expr itml-eval-1)
  (define itml-eval-descend-indent-expr itml-eval-1)
  (define itml-eval-ascend-inline-expr itml-eval-1)
  (define itml-eval-ascend-line-expr itml-eval-1)
  (define itml-eval-ascend-indent-expr itml-eval-1)

  (define (itml-ascend-proc create-result) "environment integer -> procedure"
    (l (a nesting-depth docl-state env) "list integer vhash environment -> any"
      (list (create-result a (itml-adjust-level nesting-depth) docl-state env) (- nesting-depth 1)
        docl-state env)))

  (define (itml-descend-proc create-result)
    (l (a re-descend nesting-depth docl-state env)
      (let (r (create-result a re-descend nesting-depth docl-state env))
        (if r (list r #f nesting-depth docl-state env)
          (list #f #t (+ 1 nesting-depth) docl-state env)))))

  (define (docl-itml-port->result-proc create-result)
    (l (a nesting-depth docl-state env) "read itml from a port, parse it and translate it to text"
      (docl-translate-port a
        (l (a docl-state) (create-result (port->itml-parsed a) nesting-depth docl-state env))
        docl-state)))

  (define (docl-itml-string->result-proc port->result)
    (l (a . port->result-arguments) "string _ ... -> text"
      (apply port->result (open-input-string a) port->result-arguments)))

  (define (docl-itml-parsed->result-proc itml-parsed->result)
    (l (a nesting-depth docl-state env)
      "list [integer vhash environment] -> any
      this can also be used to convert list trees with strings to html"
      (docl-translate-any a (l (a docl-state) (itml-parsed->result a nesting-depth docl-state env))
        docl-state)))

  (define
    (itml-parsed->result-proc create-result descend ascend handle-top-level-terminal
      handle-terminal)
    (l (a nesting-depth docl-state env)
      "list [integer vhash environment] -> any
      a translator for itml-parsed"
      (create-result
        (par-map
          (l (e)
            (if (list? e)
              (first
                (tree-transform-with-state e descend
                  ascend handle-terminal nesting-depth docl-state env))
              (handle-top-level-terminal e nesting-depth docl-state env)))
          a)
        nesting-depth docl-state env))))
