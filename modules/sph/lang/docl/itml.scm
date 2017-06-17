(library (sph lang docl itml)
  (export
    docl-itml-parsed->result-proc
    docl-itml-port->result-proc
    docl-itml-string->result-proc
    itml-adjust-nesting-depth
    itml-eval-ascend-indent-expr
    itml-eval-ascend-inline-expr
    itml-eval-ascend-line-expr
    itml-eval-descend-indent-expr
    itml-eval-descend-indent-scm-expr
    itml-eval-descend-inline-expr
    itml-eval-descend-inline-scm-expr
    itml-eval-descend-line-expr
    itml-eval-descend-line-scm-expr
    sph-lang-docl-itml-description)
  (import
    (ice-9 threads)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph conditional)
    (sph lang docl)
    (sph lang itml read)
    (only (guile)
      call-with-output-string
      display
      string-join
      module-ref
      read
      open-input-string)
    (only (sph lang indent-syntax) prefix-tree->indent-tree-string)
    (only (sph list) flatten simplify-list)
    (only (sph string) parenthesise)
    (only (sph tree) tree-transform-with-state))

  (define sph-lang-docl-itml-description
    "helpers to evaluate itml expressions a and convert from itml to another language")

  (define (itml-adjust-nesting-depth a)
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

  (define (itml-eval-1 a re-descend nesting-depth docl-state env)
    "list procedure integer list environment -> any
     evaluate an inline-code expression when the arguments are strings"
    (itml-list-string-eval a env nesting-depth docl-state))

  (define (itml-eval-2 a re-descend nesting-depth docl-state env)
    "evaluate an inline-code expression" (itml-list-eval a env nesting-depth docl-state))

  (define (descend->ascend-proc proc)
    "procedure -> procedure
     change the type-signature of a descend procedure to be like an ascend procedure (without the re-descend parameter)"
    (l (a . rest) (apply proc a #f rest)))

  (define itml-eval-descend-line-scm-expr itml-eval-2)
  (define itml-eval-descend-inline-scm-expr itml-eval-2)
  (define itml-eval-descend-indent-scm-expr itml-eval-2)
  (define itml-eval-descend-line-expr itml-eval-1)
  (define itml-eval-descend-indent-expr itml-eval-1)
  (define itml-eval-ascend-inline-expr (descend->ascend-proc itml-eval-1))
  (define itml-eval-ascend-line-expr (descend->ascend-proc itml-eval-1))
  (define itml-eval-ascend-indent-expr (descend->ascend-proc itml-eval-1))

  (define (docl-itml-port->result-proc create-result)
    "procedure:{list:itml-parsed integer:nesting-depth list:docl-state environment:eval-environment} -> any
     create a procedure that reads itml from a port, reads itml expressions from it and parses it to \"create-result\""
    (l (a nesting-depth docl-state env)
      "any integer list environment -> any
      read itml from a port, parse it and create a result"
      (docl-translate-port a
        (l (a docl-state) (create-result (port->itml-parsed a) nesting-depth docl-state env))
        docl-state)))

  (define (docl-itml-string->result-proc port->result)
    "procedure:{port any ... ->} -> procedure:{string any ... ->}
     creates a procedure that accepts an itml string to transform with \"port->result\""
    (l (a . port->result-arguments)
      "string _ ... -> text
      accepts an itml string to transform"
      (apply port->result (open-input-string a) port->result-arguments)))

  (define (docl-itml-parsed->result-proc itml-parsed->result)
    "create a procedure that accepts itml-parsed and pass it to \"itml-parsed->result\""
    (l (a nesting-depth docl-state env)
      "list [integer list environment] -> any
      this can also be used to convert list trees with strings to html"
      (docl-translate-any a (l (a docl-state) (itml-parsed->result a nesting-depth docl-state env))
        docl-state))))
