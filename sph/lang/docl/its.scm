;translates its syntax to html as sxml using docl

(library (sph lang docl its)
  (export
    docl-its-html-sxml-env
    docl-its-html-sxml-env-module-names
    docl-its-parsed->html-sxml
    docl-its-port->html-sxml
    docl-its-string->html-sxml
    parsed-its->html-sxml)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph lang docl)
    (sph lang docl env)
    (sph lang docl env its-to-html-sxml)
    (sph lang parser its)
    (sph read-write)
    (only (sph list) insert-second)
    (only (sph one) string->datum first-as-result)
    (only (sph string) string-equal?)
    (only (sph tree)
      flatten
      tree-transform-with-state
      tree-fold-reverse-with-level))

  (define-syntax-rule (with-level a-level expr)
    (begin (docl-env-set! (q indent-depth) a-level)
      (first-as-result expr (docl-env-set! (q indent-depth) #f))))

  (define docl-its-html-sxml-env-module-names
    (pair (q (sph lang docl env its-to-html-sxml)) docl-default-env-module-names))

  (define docl-its-html-sxml-env (apply environment docl-its-html-sxml-env-module-names))

  (define-syntax-rule (join-heading-section a level)
    (section* level (first a) (add-paragraphs (tail a))))

  (define-syntax-rule (heading-section? a)
    (and (list? a) (> (length a) 1) (not (eqv? (q section) (first a)))))

  (define-syntax-rule (list->sxml a level level-init)
    (if (heading-section? a) (join-heading-section a level) a))

  (define-syntax-rule (ascend-expr->sxml prefix content ele env level level-init)
    (case prefix ((line) (add-spaces content))
      ;eval is used so that even syntax works.
      ;level is decremented for correction
      ( (inline-expr)
        (call-for-eval level
          (l () (eval (list (string->symbol (first content)) (list (q quote) (tail content))) env))))
      ( (line-expr)
        (call-for-eval level
          (l () (eval (pair (string->symbol (first content)) (tail content)) env))))
      ( (indent-expr)
        (call-for-eval level
          (l () ((module-ref env (string->symbol (first content))) (tail content)))))
      ((association) (list (q dl) (list (q dt) (let (e (first content)) (if (string? e) (string-append e ":") e))) (pair (q dd) (tail content))))
      (else (list->sxml ele level level-init))))

  (define (ascend-proc env level-init)
    (l (e level)
      (list
        (let ((prefix (first e)) (content (tail e)))
          (ascend-expr->sxml prefix content e env level level-init))
        (- level 1))))

  (define (call-for-eval level c) (docl-env-set! (q indent-depth) (- level 1))
    (let (r (c)) (docl-env-set! (q indent-depth) #f) r))

  (define (descend-expr->sxml a re-descend level env)
    (case (first a)
      ( (inline-scm-expr)
        (call-for-eval level (l () (eval (string->datum (first (tail a)) read) env))))
      ( (indent-descend-expr)
        (call-for-eval level
          (l ()
            (let* ((content (tail a)) (prefix (first content)))
              ( (module-ref env (if (string-equal? "#" prefix) (q escape) (string->symbol prefix)))
                (tail content))))))
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
        ;display converts a string-list to a scheme-expression with symbols. flatten is used to ignore the indent-tree nesting
        (call-for-eval level
          (l ()
            (eval
              (string->datum (call-with-output-string (l (port) (display (flatten (tail a)) port))))
              env))))
      (else #f)))

  (define (descend-proc env level-init)
    (l (a re-descend level)
      (let (result (descend-expr->sxml a re-descend level env))
        (if result (list result #f level) (list #f #t (+ 1 level))))))

  (define* (parsed-its->html-sxml a env #:optional (level-init 0))
    "a translator for parsed-its. (does not depend on docl)"
    (add-paragraphs
      (map
        (l (e)
          (if (list? e)
            (first
              (tree-transform-with-state e (descend-proc env level-init)
                (ascend-proc env level-init) (l a a) level-init))
            (if (eqv? (q line) e) (q (br)) e)))
        a)))

  (define*
    (docl-its-parsed->html-sxml input #:optional bindings keep-prev-bindings
      (env docl-its-html-sxml-env)
      (level-init 0))
    "list [symbol-hashtable/boolean boolean environment integer] -> sxml
    translate parsed"
    (docl-translate-any input
      (l (input) (parsed-its->html-sxml input env (or (docl-env-ref (q indent-depth)) level-init)))
      bindings keep-prev-bindings))

  (define*
    (docl-its-port->html-sxml input #:optional bindings keep-prev-bindings
      (env docl-its-html-sxml-env)
      (level-init 0))
    "port [symbol-hashtable/boolean boolean environment integer] -> sxml
    read its from a port, parse it and translate it to html-sxml"
    (docl-translate-port input
      (l (input)
        (parsed-its->html-sxml (port->parsed-its input) env
          (or (docl-env-ref (q indent-depth)) level-init)))
      bindings keep-prev-bindings))

  (define (docl-its-string->html-sxml input . docl-its-port->html-sxml-args)
    "string [symbol-hashtable/boolean boolean environment integer] -> sxml"
    (apply docl-its-port->html-sxml (open-input-string input) docl-its-port->html-sxml-args)))