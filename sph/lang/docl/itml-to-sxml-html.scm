(library (sph lang docl itml-to-sxml-html)
  (export
    docl-itml-parsed->sxml-html
    docl-itml-port->sxml-html
    docl-itml-string->sxml-html
    docl-itml-sxml-html-env
    docl-itml-sxml-html-env-module-names
    parsed-itml->sxml-html)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph lang docl)
    (sph lang docl env)
    (sph lang docl env itml-to-sxml-html)
    (sph lang parser itml)
    (sph list)
    (sph read-write)
    (only (sph one) string->datum first-as-result)
    (only (sph string) string-equal?)
    (only (sph tree) flatten tree-transform-with-state)
    (only (srfi srfi-1) remove))

  (define docl-itml-sxml-html-env-module-names
    (pair (q (sph lang docl env itml-to-sxml-html)) docl-default-env-module-names))

  (define docl-itml-sxml-html-env (apply environment docl-itml-sxml-html-env-module-names))

  (define-syntax-rule (join-heading-section a level)
    (section* level (first a) (process-top-level-lines (tail a))))

  (define-syntax-rule (heading-section? a)
    (and (list? a) (> (length a) 1) (not (eqv? (q section) (first a)))))

  (define-syntax-rule (list->sxml a level level-init)
    (if (heading-section? a) (join-heading-section a level) a))

  (define-syntax-rule (ascend-expr->sxml prefix content e env level level-init)
    (case prefix ((line) (if (null? content) "" (add-spaces content)))
      ;eval is used to support macros in itml-expressions
      ( (inline-expr)
        (call-for-eval level
          (thunk (eval (list (string->symbol (first content)) (list (q quote) (tail content))) env))))
      ( (line-expr)
        (call-for-eval level
          (thunk (eval (pair (string->symbol (first content)) (tail content)) env))))
      ( (indent-expr)
        (call-for-eval level
          (thunk ((module-ref env (string->symbol (first content))) (tail content)))))
      ((association) (pairs (first content) ": " (tail content)))
      (else (list->sxml e level level-init))))

  (define (adjust-level a)
    "integer -> integer
    level 0 and 1 are equivalent because content of nested-lists on the top-level in
    parsed-itml is still considered belonging to the top-level"
    (max 0 (- a 1)))

  (define (ascend-proc env level-init)
    (l (e level)
      (list
        (let ((prefix (first e)) (content (tail e)))
          (ascend-expr->sxml prefix content e env (adjust-level level) level-init))
        (- level 1))))

  (define (call-for-eval level c) (docl-env-set! (q nesting-level) level)
    (let (r (c)) (docl-env-set! (q nesting-level) #f) r))

  (define (descend-expr->sxml a re-descend level env)
    (case (first a)
      ( (inline-scm-expr)
        (call-for-eval level (thunk (eval (string->datum (first (tail a)) read) env))))
      ( (indent-descend-expr)
        (call-for-eval level
          (thunk
            (let* ((content (tail a)) (prefix (first content)))
              ( (if (string-equal? "#" prefix) escape (module-ref env (string->symbol prefix)))
                (tail content) level)))))
      ( (line-scm-expr)
        (call-for-eval level
          (thunk
            (eval
              (string->datum
                (string-append
                  (let (content (tail a))
                    (apply string-append "(" (first content) " " (tail content)))
                  ")")
                read)
              env))))
      ( (indent-scm-expr)
        (call-for-eval level
          (thunk
            (eval
              (string->datum
                (call-with-output-string
                  (l (port)
                    ;display converts a string-list to a scheme-expression with symbols. flatten is used to ignore the indent-tree nesting
                    (display (flatten (tail a)) port))))
              env))))
      (else #f)))

  (define (descend-proc env level-init)
    (l (a re-descend level)
      (let (r (descend-expr->sxml a re-descend level env))
        (if r (list r #f level) (list #f #t (+ 1 level))))))

  (define (tag-element? a)
    "list:non-null-list -> boolean
    top-level-lines are everything on the top-level except lists with symbols as the first element"
    (and (list? a) (symbol? (first a))))

  (define html-tags-inline
    (ql span a br object img script select button input label select textarea))

  (define (html-tag-inline? a) (and (symbol? a) (containsv? html-tags-inline a)))

  (define (process-top-level-lines a)
    "list integer -> list
    the top-level is interpreted as a list of lines. this procedure inserts line breaks between string or symbol elements.
    removes empty lists and merges sub-lists that are not tag elements.
    sub-expressions creators are supposed to handle line breaks themselves"
    (if (null? a) a
      (let (e (first a))
        ( (if (list? e)
            (if (null? e) (l (e r) r)
              (let (prefix (first e))
                (if (symbol? prefix)
                  (if (containsv? html-tags-inline prefix)
                    (l (e r) (if (null? r) (pair e r) (pairs e (list (ql br)) r))) pair)
                  (l (e r) (if (null? r) (append e r) (append e (list (ql br)) r))))))
            (if (null? (tail a)) pair
              (let (e-next (first a))
                (if (tag-element? e-next) pair
                  (l (e r) (if (null? r) (pair e r) (pairs e (ql br) r)))))))
          e (process-top-level-lines (tail a))))))

  (define-syntax-rule (handle-line-empty a) (if (eqv? (q line-empty) a) (ql br) a))
  (define (handle-terminal a level-init) (list (handle-line-empty a) level-init))

  (define* (parsed-itml->sxml-html a env #:optional (level-init 0))
    "list environment [integer] -> sxml
    a translator for parsed-itml. does not depend on docl"
    (process-top-level-lines
      (map
        (l (e)
          (if (list? e)
            (first
              (tree-transform-with-state e (descend-proc env level-init)
                (ascend-proc env level-init) handle-terminal level-init))
            (handle-line-empty e)))
        a)))

  (define*
    (docl-itml-parsed->sxml-html input #:optional bindings keep-prev-bindings
      (env docl-itml-sxml-html-env)
      (level-init 0))
    "list [symbol-hashtable/boolean boolean environment integer] -> sxml
    this can also be used to convert list trees with strings to html"
    (docl-translate-any input
      (l (input)
        (parsed-itml->sxml-html input env (or (docl-env-ref (q nesting-level)) level-init)))
      bindings keep-prev-bindings))

  (define*
    (docl-itml-port->sxml-html input #:optional bindings keep-prev-bindings
      (env docl-itml-sxml-html-env)
      (level-init 0))
    "port [symbol-hashtable/boolean boolean environment integer] -> sxml
    read itml from a port, parse it and translate it to sxml-html"
    (docl-translate-port input
      (l (input) (parsed-itml->sxml-html (port->parsed-itml input) env (current-nesting-level)))
      bindings keep-prev-bindings))

  (define (docl-itml-string->sxml-html input . docl-itml-port->sxml-html-args)
    "string [symbol-hashtable/boolean boolean environment integer] -> sxml"
    (apply docl-itml-port->sxml-html (open-input-string input) docl-itml-port->sxml-html-args)))
