(library (sph lang itml write)
  (export
    itml-create-association
    itml-create-indent-expr
    itml-create-indent-scm-expr
    itml-create-inline-expr
    itml-create-inline-scm-expr
    itml-create-line-expr
    itml-create-line-scm-expr
    itml-parsed->itml
    sph-lang-itml-write-description)
  (import
    (guile)
    (ice-9 threads)
    (sph)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph lang itml eval)
    (sph lang itml read)
    (sph list)
    (sph string)
    (sph tree))

  (define sph-lang-itml-write-description "create itml strings from parsed itml")
  (define prefix-expr-scm "\\.")
  (define prefix-expr "\\")

  (define (itml-create-line-scm-expr a)
    (let (a (map any->string-write a))
      (string-append prefix-expr-scm (first a) ": " (string-join (tail a) " "))))

  (define (itml-create-inline-scm-expr a) (string-append prefix-expr-scm (any->string-write a)))

  (define (itml-create-indent-scm-expr a)
    (let (a (tree-map-leafs any->string-write a))
      (string-append prefix-expr-scm (prefix-tree->indent-tree-string (list a)))))

  (define (itml-create-inline-expr a) (string-append prefix-expr (any->string-display a)))

  (define (itml-create-indent-expr a)
    (let (a (tree-map-leafs any->string-display a))
      (string-append prefix-expr (prefix-tree->indent-tree-string (list a)))))

  (define (itml-create-line-expr a)
    (let (a (map any->string-display a))
      (string-append prefix-expr (first a) ": " (string-join (tail a) " "))))

  (define (itml-create-association a . b) (apply string-append a ": " b))
  (define (ascend-handle-line a depth) (apply string-append a))
  (define (descend-handle-double-backslash a depth) "\\\\")
  (define (ascend-handle-association a depth) (apply string-append (first a) ": " (tail a)))
  (define (handle-descend-line-scm-expr a depth) (itml-create-line-scm-expr a))
  (define (handle-descend-inline-scm-expr a depth) (apply string-append prefix-expr-scm a))

  (define (handle-descend-indent-scm-expr a depth)
    (pair (string-append prefix-expr-scm (first a)) (tail a)))

  (define (handle-descend-indent-expr a depth) (pair (string-append "\\#" (first a)) (tail a)))
  (define (handle-ascend-inline-expr a depth) (string-append prefix-expr (any->string-display a)))

  (define (string-char-escape a)
    "string -> string
     add the backslash escape character in front of every character that needs to be escaped in itml.
     escaping is only necessary on ascend or for terminals,
     because arguments to descend expressions are interpreted as plain text and do not need to escape these chars"
    (let
      (spec
        (remove not
          (list (if (string-contains-char? a #\\) (list #\\ #\\ #\\) #f)
            (if (string-contains-char? a #\:) (list #\: #\\ #\:) #f))))
      (if (null? spec) a (string-replace-chars a spec))))

  (define (list-string-char-escape a) "list -> list" (map-selected string? string-char-escape a))

  (define (handle-ascend-line-expr a depth)
    (apply string-append prefix-expr (first a) ": " (list-string-char-escape (tail a))))

  (define (handle-ascend-indent-expr a depth)
    (pair (string-append prefix-expr (first a)) (list-string-char-escape (tail a))))

  (define-as ascend-prefix->handler-ht ht-create-symbol
    line ascend-handle-line
    inline-expr handle-ascend-inline-expr
    line-expr handle-ascend-line-expr
    indent-expr handle-ascend-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht ht-create-symbol
    inline-scm-expr handle-descend-inline-scm-expr
    line-scm-expr handle-descend-line-scm-expr
    indent-scm-expr handle-descend-indent-scm-expr
    indent-descend-expr handle-descend-indent-expr double-backslash descend-handle-double-backslash)

  (define-syntax-rule (expr->itml prefix->handler a proc-arguments ...)
    (let (p (ht-ref prefix->handler (first a))) (and p (p (tail a) proc-arguments ...))))

  (define (ascend-expr->itml a depth) (or (expr->itml ascend-prefix->handler-ht a depth) a))
  (define (descend-expr->itml a re-descend depth) (expr->itml descend-prefix->handler-ht a depth))

  (define (handle-top-level-terminal a . states)
    (if (string? a) (string-char-escape a) (if (eqv? (q line-empty) a) "" a)))

  (define (handle-terminal a . states) (pair (handle-top-level-terminal a) states))

  (define* (itml-parsed->itml a #:optional (depth 0))
    (prefix-tree->indent-tree-string
      (itml-eval a (itml-state-create depth #f)
        (l (a . b) (apply prefix-dispatch descend-prefix-ht a b))
        ; ascend
        (l (a . b) (or (apply prefix-dispatch ascend-prefix-ht a b) (apply ascend-list a b)))
        ; terminal
        (l (a . b) (if (eq? (q line-empty) a) (q (br)) a)) (itml-descend-proc descend-expr->itml)
        (itml-ascend-proc ascend-expr->itml identity) handle-top-level-terminal handle-terminal)
      depth)))
