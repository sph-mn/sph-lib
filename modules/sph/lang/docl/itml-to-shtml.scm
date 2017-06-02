(library (sph lang docl itml-to-shtml)
  (export
    docl-itml-env-shtml
    docl-itml-env-shtml-module-names
    docl-itml-parsed->shtml
    docl-itml-port->shtml
    docl-itml-string->shtml
    docl-itml-shtml-env-module-names
    itml-parsed->shtml
    process-lines)
  (import
    (guile)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph lang docl)
    (sph lang docl env itml-to-shtml)
    (sph lang docl itml)
    (sph lang itml)
    (sph lang itml read)
    (sph list)
    (sph read-write)
    (sph set)
    (sph web shtml)
    (only (sph hashtable) hashtable-ref symbol-hashtable)
    (only (sph string) string-equal?)
    (only (sph tree) flatten tree-transform-with-state)
    (only (srfi srfi-1) remove))

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
    (shtml-section nesting-depth (first a) (process-lines (tail a))))

  (define-syntax-rule (heading-section? a)
    (and (list? a) (> (length a) 1) (not (eqv? (q section) (first a)))))

  (define-syntax-rule (list->sxml a nesting-depth)
    (if (heading-section? a) (join-heading-section a nesting-depth) a))

  (define docl-itml-env-shtml-module-names
    (pair (q (sph lang docl env itml-to-shtml)) docl-default-env-module-names))

  (define docl-itml-env-shtml (apply environment docl-itml-env-shtml-module-names))
  (define (ascend-handle-line a nesting-depth docl-state env) (if (null? a) "" a))
  (define (descend-handle-double-backslash a re-descend nesting-depth docl-state env) "\\")

  (define (descend-handle-association a re-descend nesting-depth docl-state env)
    (let
      ( (keyword (first a))
        (re-descend*
          (l (keyword)
            (map (compose first (l (e) (re-descend e nesting-depth docl-state env)))
              (any->list keyword)))))
      (append (if (string? keyword) (list keyword) (re-descend* keyword))
        (pair ": " (re-descend* (tail a))))))

  (define (descend-handle-escaped-association-infix a re-descend nesting-depth docl-state env) ": ")

  (define-as ascend-prefix->handler-ht symbol-hashtable
    line ascend-handle-line
    inline-expr itml-eval-ascend-inline-expr
    line-expr itml-eval-ascend-line-expr indent-expr itml-eval-ascend-indent-expr)

  (define-as descend-prefix->handler-ht symbol-hashtable
    inline-scm-expr itml-eval-descend-inline-scm-expr
    line-scm-expr itml-eval-descend-line-scm-expr
    association descend-handle-association
    indent-scm-expr itml-eval-descend-indent-scm-expr
    indent-descend-expr itml-eval-descend-indent-expr
    escaped-association-infix descend-handle-escaped-association-infix
    double-backslash descend-handle-double-backslash)

  (define-syntax-rule (expr->shtml prefix->handler a proc-arguments ...)
    (let (p (hashtable-ref prefix->handler (first a))) (and p (p (tail a) proc-arguments ...))))

  (define (ascend-expr->shtml a nesting-depth docl-state env)
    (or (expr->shtml ascend-prefix->handler-ht a nesting-depth docl-state env)
      (list->sxml a nesting-depth)))

  (define (descend-expr->shtml a re-descend nesting-depth docl-state env)
    (expr->shtml descend-prefix->handler-ht a re-descend nesting-depth docl-state env))

  (define (handle-top-level-terminal a . states) (if (eqv? (q line-empty) a) (ql br) a))
  (define (handle-terminal a . states) (pair (handle-top-level-terminal a) states))

  (define itml-parsed->shtml
    (itml-parsed->result-proc (l (a nesting-depth docl-state env) (process-lines a))
      (itml-descend-proc descend-expr->shtml)
      (itml-ascend-proc ascend-expr->shtml itml-adjust-nesting-depth) handle-top-level-terminal
      handle-terminal))

  (define docl-itml-parsed->shtml (docl-itml-parsed->result-proc itml-parsed->shtml))
  (define docl-itml-port->shtml (docl-itml-port->result-proc itml-parsed->shtml))
  (define docl-itml-string->shtml
    ;same signature as docl-itml-port->result-proc
    (docl-itml-string->result-proc docl-itml-port->shtml)))
