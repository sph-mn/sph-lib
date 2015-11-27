(library (sph lang docl itml-to-sxml-html)
  (export
    docl-itml-env-sxml-html
    docl-itml-env-sxml-html-module-names
    docl-itml-parsed->sxml-html
    docl-itml-port->sxml-html
    docl-itml-string->sxml-html
    docl-itml-sxml-html-env-module-names
    itml-parsed->sxml-html
    process-lines
    section
    sxml-html-heading)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph lang docl)
    (sph lang itml)
    (sph lang docl env itml-to-sxml-html)
    (sph lang docl itml)
    (sph lang itml read)
    (sph list)
    (sph read-write)
    (sph set)
    (only (sph hashtable) hashtable-ref symbol-hashtable)
    (only (sph one) string->datum first-as-result)
    (only (sph string) string-equal?)
    (only (sph tree) flatten tree-transform-with-state)
    (only (srfi srfi-1) remove))

  (define html-headings (q #(h1 h2 h3 h4 h5 h6)))

  (define (sxml-html-heading nesting-depth . content)
    (pair (vector-ref html-headings (min 5 nesting-depth)) content))

  (define (section nesting-depth title content . attributes)
    (pair (q section)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (sxml-html-heading nesting-depth title)
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content) (list (pair (q div) content))))
            (list (list (q div) content)))))))

  (define (add-spaces a)
    "list -> list
    inserts a space before non-list elements (strings, numbers, etc) except the first and splices lists of expressions."
    (fold-right
      (l (e r)
        (if (list? e) (if (null? e) (pair e r) (if (symbol? (first e)) (pair e r) (append e r)))
          (if (null? r) (pair e r)
            (if (and (string? e) (string-suffix? " " e)) (pair e r) (pairs e " " r)))))
      (list) a))

  (define (string->sxml a nesting-level docl-state)
    "string -> sxml
    convert newlines in string to (br) and result in an sxml expression"
    (let (a (string-split a #\newline)) (if (length-eq-one? a) (first a) (interleave a (ql br)))))

  (define (tag-element? a)
    "list:non-null-list -> boolean
    top-level-lines are everything on the top-level except lists with symbols as the first element"
    (and (list? a) (symbol? (first a))))

  (define html-tags-no-newline
    (apply set-symbol-create
      (ql span a object img script select button input label select textarea)))

  (define-syntax-rule (html-tag-no-newline? a) (hashtable-ref html-tags-no-newline a))
  (define-syntax-rule (handle-line a) (list (q p) a))
  (define-syntax-rule (handle-line-list a) (pair (q p) a))

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

  (define-syntax-rule (join-heading-section a nesting-depth)
    (section nesting-depth (first a) (process-lines (tail a))))

  (define-syntax-rule (heading-section? a)
    (and (list? a) (> (length a) 1) (not (eqv? (q section) (first a)))))

  (define-syntax-rule (list->sxml a nesting-depth)
    (if (heading-section? a) (join-heading-section a nesting-depth) a))

  (define docl-itml-env-sxml-html-module-names
    (pair (q (sph lang docl env itml-to-sxml-html)) docl-default-env-module-names))

  (define docl-itml-env-sxml-html (apply environment docl-itml-env-sxml-html-module-names))
  (define (ascend-handle-line a nesting-depth docl-state env) (if (null? a) "" (add-spaces a)))
  (define (descend-handle-double-backslash a nesting-depth docl-state env) "\\")

  (define (ascend-handle-association a nesting-depth docl-state env)
    (pairs (first a) ": " (tail a)))

  ;escaped characters are added as new line

  (define (descend-handle-escaped-association-infix a nesting-depth docl-state env)
    ": ")


  (define-as ascend-prefix->handler-ht symbol-hashtable
    line ascend-handle-line
    inline-expr itml-eval-ascend-inline-expr
    line-expr itml-eval-ascend-line-expr
    indent-expr itml-eval-ascend-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht symbol-hashtable
    inline-scm-expr itml-eval-descend-inline-scm-expr
    line-scm-expr itml-eval-descend-line-scm-expr
    indent-scm-expr itml-eval-descend-indent-scm-expr
    indent-descend-expr itml-eval-descend-indent-expr
    escaped-association-infix descend-handle-escaped-association-infix
    double-backslash descend-handle-double-backslash)

  (define-syntax-rule (expr->sxml-html prefix->handler a proc-arguments ...)
    (let (p (hashtable-ref prefix->handler (first a))) (and p (p (tail a) proc-arguments ...))))

  (define (ascend-expr->sxml-html a nesting-depth docl-state env)
    (or (expr->sxml-html ascend-prefix->handler-ht a nesting-depth docl-state env)
      (list->sxml a nesting-depth)))

  (define (descend-expr->sxml-html a re-descend nesting-depth docl-state env)
    (expr->sxml-html descend-prefix->handler-ht a nesting-depth docl-state env))

  (define (handle-top-level-terminal a . states) (if (eqv? (q line-empty) a) (ql br) a))
  (define (handle-terminal a . states) (pair (handle-top-level-terminal a) states))

  (define itml-parsed->sxml-html
    (itml-parsed->result-proc (l (a nesting-depth docl-state env) (process-lines a))
      (itml-descend-proc descend-expr->sxml-html)
      (itml-ascend-proc ascend-expr->sxml-html itml-adjust-nesting-depth) handle-top-level-terminal
      handle-terminal))

  (define docl-itml-parsed->sxml-html (docl-itml-parsed->result-proc itml-parsed->sxml-html))
  (define docl-itml-port->sxml-html (docl-itml-port->result-proc itml-parsed->sxml-html))
  (define docl-itml-string->sxml-html (docl-itml-string->result-proc docl-itml-port->sxml-html)))
