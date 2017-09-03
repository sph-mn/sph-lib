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

  (define* (itml-create-indent-scm-expr a #:optional line-width)
    (string-append prefix-expr-scm
      (prefix-tree->indent-tree
        (list
          (tree-map
            (l (a)
              (if (list? a)
                (if line-width
                  (let (a-string (any->string-display a))
                    (if (<= (string-length a-string) line-width) a-string a))
                  a)
                (any->string-write a)))
            a)))))

  (define (itml-create-inline-expr a) (string-append prefix-expr (any->string-display a)))

  (define (itml-create-indent-expr a)
    (let (a (tree-map-leafs any->string-display a))
      (string-append prefix-expr (prefix-tree->indent-tree (list a)))))

  (define (itml-create-line-expr a)
    (let (a (map any->string-display a))
      (string-append prefix-expr (first a) ": " (string-join (tail a) " "))))

  (define (itml-create-association a . b) (apply string-append a ": " b))

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

  (define-as ascend-ht ht-create-symbol
    line (l (a . b) (apply string-append a))
    inline-expr (l (a . b) (string-append prefix-expr (any->string-display a)))
    line-expr
    (l (a . b) (apply string-append prefix-expr (first a) ": " (list-string-char-escape (tail a))))
    indent-expr
    (l (a . b) (pair (string-append prefix-expr (first a)) (list-string-char-escape (tail a))))
    association (l (a . b) (apply string-append (first a) ": " (tail a))))

  (define-as descend-ht ht-create-symbol
    inline-scm-expr (l (a . b) (string-append prefix-expr-scm (any->string a)))
    line-scm-expr (l (a . b) (itml-create-line-scm-expr a))
    indent-scm-expr (l (a . b) (itml-create-indent-scm-expr (pair (first a) (tail a)) 80))
    indent-descend-expr (l (a . b) (pair (string-append "\\#" (first a)) (tail a)))
    double-backslash (l (a . b) "\\\\"))

  (define itml-parsed->itml
    (let
      (eval
        (itml-eval* descend-ht ascend-ht
          (l (a . b) (if (string? a) (string-char-escape a) (if (eqv? (q line-empty) a) "" a)))))
      (l (a) "list list -> sxml" (prefix-tree->indent-tree (eval a (itml-state-create)) 0)))))
