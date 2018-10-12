(library (sph lang itml write)
  (export
    itml-create-association
    itml-create-indent-scm-expression
    itml-create-indent-text-expression
    itml-create-inline-scm-expression
    itml-create-inline-text-expression
    itml-create-line-scm-expression
    itml-create-line-text-expression
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
  (define prefix-expression-scm "#")
  (define prefix-expression "##")

  (define (itml-create-line-scm-expression a)
    (let (a (map any->string-write a))
      (string-append prefix-expression-scm (first a) ": " (string-join (tail a) " "))))

  (define (itml-create-inline-scm-expression a)
    (string-append prefix-expression-scm (any->string-write a)))

  (define* (itml-create-indent-scm-expression a #:optional line-width)
    (string-append prefix-expression-scm
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

  (define (itml-create-inline-text-expression a)
    (string-append prefix-expression (any->string-display a)))

  (define (itml-create-indent-text-expression a)
    (let (a (tree-map-leafs any->string-display a))
      (string-append prefix-expression (prefix-tree->indent-tree (list a)))))

  (define (itml-create-line-text-expression a)
    (let (a (map any->string-display a))
      (string-append prefix-expression (first a) ": " (string-join (tail a) " "))))

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

  (define-as ascend-ht ht-create-symbol-q
    line (l (a . b) (apply string-append a))
    inline-text-expression (l (a . b) (string-append prefix-expression (any->string-display a)))
    line-text-expression
    (l (a . b)
      (apply string-append prefix-expression (first a) ": " (list-string-char-escape (tail a))))
    indent-text-expression
    (l (a . b)
      (pair (string-append prefix-expression (first a)) (list-string-char-escape (tail a))))
    association (l (a . b) (apply string-append (first a) ": " (tail a))))

  (define-as descend-ht ht-create-symbol-q
    inline-scm-expression (l (a . b) (string-append prefix-expression-scm (any->string a)))
    line-scm-expression (l (a . b) (itml-create-line-scm-expression a))
    indent-scm-expression
    (l (a . b) (itml-create-indent-scm-expression (pair (first a) (tail a)) 80))
    indent-descend-expression (l (a . b) (pair (string-append "###" (first a)) (tail a)))
    double-backslash (l (a . b) "\\\\"))

  (define itml-parsed->itml
    (let
      (eval
        (itml-eval* descend-ht ascend-ht
          #:terminal
          (l (a . b) (if (string? a) (string-char-escape a) (if (eqv? (q line-empty) a) "" a)))))
      (l (a) "list list -> sxml" (prefix-tree->indent-tree (eval a (itml-state-create)) 0)))))
