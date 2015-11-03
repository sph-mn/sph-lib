
(library (sph lang docl itml-to-sxml-html)
  (export
    docl-itml-sxml-html-env
    docl-itml-sxml-html-env-module-names
    docl-itml-parsed->sxml-html
    docl-itml-port->sxml-html
    docl-itml-string->sxml-html
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
    (sph read-write)
    (only (sph list) insert-second)
    (only (sph one) string->datum first-as-result)
    (only (sph string) string-equal?)
    (only (sph tree) flatten tree-transform-with-state))

  ;translates indent-tree-markup-language to html as sxml using docl.
  ;the html is supposed to look almost the same in browsers that support css and text browsers that do not support css.
  ;all the "indent" handling is only done because current text browsers do not support css yet.

  (define docl-itml-sxml-html-env-module-names
    (pair (q (sph lang docl env itml-to-sxml-html)) docl-default-env-module-names))

  (define docl-itml-sxml-html-env (apply environment docl-itml-sxml-html-env-module-names))

  (define-syntax-rule (join-heading-section a level)
    (section* level (first a) (add-paragraphs-and-indent (tail a) (+ 1 level))))

  (define-syntax-rule (heading-section? a)
    (and (list? a) (> (length a) 1) (not (eqv? (q section) (first a)))))

  (define-syntax-rule (list->sxml a level level-init)
    (if (heading-section? a) (join-heading-section a level) a))

  (define-syntax-rule (ascend-expr->sxml prefix content e env level level-init)
    (case prefix ((line) (add-spaces content))
      ;eval is used so that syntax forms work in docl expressions.
      ( (inline-expr)
        (call-for-eval level
          (l () (eval (list (string->symbol (first content)) (list (q quote) (tail content))) env))))
      ( (line-expr)
        (call-for-eval level
          (l () (eval (pair (string->symbol (first content)) (tail content)) env))))
      ( (indent-expr)
        (call-for-eval level
          (l () ((module-ref env (string->symbol (first content))) (tail content)))))
      ( (association)
        (pair (let (e (first content)) (if (string? e) (string-append e ": ") e)) (tail content)))
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

  (define (call-for-eval level c) (docl-env-set! (q indent-depth) level)
    (let (r (c)) (docl-env-set! (q indent-depth) #f) r))

  (define (descend-expr->sxml a re-descend level env)
    (case (first a)
      ( (inline-scm-expr)
        (call-for-eval level (l () (eval (string->datum (first (tail a)) read) env))))
      ( (indent-descend-expr)
        (call-for-eval level
          (l ()
            (let* ((content (tail a)) (prefix (first content)))
              ( (module-ref env
                  (if (string-equal? "#" prefix) (q escape-with-indent) (string->symbol prefix)))
                (tail content) level)))))
      ( (line-scm-expr)
        (call-for-eval level
          (l ()
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
          (l ()
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

  (define* (parsed-itml->sxml-html a env #:optional (level-init 0))
    "list environment [integer] -> sxml
    a translator for parsed-itml. does not depend on docl"
    (add-paragraphs-and-indent
      (map
        (l (e)
          (if (list? e)
            (first
              (tree-transform-with-state e (descend-proc env level-init)
                (ascend-proc env level-init) (l a a) level-init))
            (if (eqv? (q line) e) (q (br)) e)))
        a)
      level-init))

  (define*
    (docl-itml-parsed->sxml-html input #:optional bindings keep-prev-bindings
      (env docl-itml-sxml-html-env)
      (level-init 0))
    "list [symbol-hashtable/boolean boolean environment integer] -> sxml
    this can also be used to convert list trees with strings to html"
    (docl-translate-any input
      (l (input) (parsed-itml->sxml-html input env (or (docl-env-ref (q indent-depth)) level-init)))
      bindings keep-prev-bindings))

  (define*
    (docl-itml-port->sxml-html input #:optional bindings keep-prev-bindings
      (env docl-itml-sxml-html-env)
      (level-init 0))
    "port [symbol-hashtable/boolean boolean environment integer] -> sxml
    read itml from a port, parse it and translate it to sxml-html"
    (docl-translate-port input
      (l (input)
        (parsed-itml->sxml-html (port->parsed-itml input) env
          (or (docl-env-ref (q indent-depth)) level-init)))
      bindings keep-prev-bindings))

  (define (docl-itml-string->sxml-html input . docl-itml-port->sxml-html-args)
    "string [symbol-hashtable/boolean boolean environment integer] -> sxml"
    (apply docl-itml-port->sxml-html (open-input-string input) docl-itml-port->sxml-html-args)))