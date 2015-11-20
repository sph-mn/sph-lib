(library (sph lang docl itml-to-sxml-html)
  (export
    docl-itml-parsed->sxml-html
    docl-itml-port->sxml-html
    docl-itml-string->sxml-html
    docl-itml-sxml-html-env
    docl-itml-sxml-html-env-module-names
    parsed-itml->sxml-html
    process-lines)
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
    (sph set)
    (only (sph hashtable) hashtable-ref)
    (only (sph one) string->datum first-as-result)
    (only (sph string) string-equal?)
    (only (sph tree) flatten tree-transform-with-state)
    (only (srfi srfi-1) remove))

  (define docl-itml-sxml-html-env-module-names
    (pair (q (sph lang docl env itml-to-sxml-html)) docl-default-env-module-names))

  (define docl-itml-sxml-html-env (apply environment docl-itml-sxml-html-env-module-names))

  (define (ascend-expr->text prefix content e env level level-init)
    (case prefix ((line) (ascend-handle-line e content level env))
      ((inline-expr) (ascend-eval-inline-expr e content level env))
      ((line-expr) (ascend-eval-line-expr e content level env))
      ((indent-expr) (ascend-eval-indent-expr e content level env))
      ((association) (ascend-handle-association e content level env)) (else e)))

  (define (descend-expr->text a re-descend level env)
    (pass-if
      (case (first a) ((inline-scm-expr) (descend-eval-inline-scm-expr (tail a) level env))
        ((indent-descend-expr) (descend-eval-indent-descend-expr (tail a) level env))
        ((line-scm-expr) (descend-eval-line-scm-expr (tail a) level env))
        ((indent-scm-expr) (descend-eval-indent-scm-expr (tail a) level env)) (else #f))
      any->string))

  (define parsed-itml->text
    (parsed-itml->result-proc (descend-proc-proc descend-expr->text)
      (ascend-proc-proc ascend-expr->text) prefix-tree->indent-tree-string (const "")))

  (define docl-itml-parsed->text
    (docl-itml-parsed->result-proc parsed-itml->text docl-itml-text-env))

  (define docl-itml-port->text (docl-itml-port->result-proc parsed-itml->text docl-itml-text-env))
  (define docl-itml-string->text (docl-itml-string->result-proc docl-itml-port->text))

  (define-syntax-rule (join-heading-section a level)
    (section* level (first a) (process-lines (tail a))))

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

  (define html-tags-no-newline
    (apply set-symbol-create
      (ql span a object img script select button input label select textarea)))

  (define (html-tag-no-newline? a) (hashtable-ref html-tags-no-newline a))
  (define (handle-line a) (list (q p) a))
  (define (handle-line-list a) (pair (q p) a))

  (define (process-lines a)
    "list integer -> list
    the top-level is interpreted as a list of lines. this procedure inserts line breaks between string or symbol elements.
    removes empty lists and merges sub-lists that are not tag elements.
    sub-expressions creators are supposed to handle line breaks themselves"
    (if (null? a) a
      (let ((e (first a)) (r (process-lines (tail a))))
        (if (list? e)
          (if (null? e) r
            (let (prefix (first e))
              (pair
                (if (symbol? prefix) (if (html-tag-no-newline? prefix) (handle-line e) e)
                  (handle-line-list e))
                r)))
          (pair (handle-line e) r)))))

  (define-syntax-rule (handle-line-empty a) (if (eqv? (q line-empty) a) (ql br) a))
  (define (handle-terminal a level-init) (list (handle-line-empty a) level-init))

  (define* (parsed-itml->sxml-html a env #:optional (level-init 0))
    "list environment [integer] -> sxml
    a translator for parsed-itml. does not depend on docl"
    (process-lines
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
