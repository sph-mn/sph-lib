(library (sph lang scm-format)
  (export
    ascend-prefix->format-proc
    default-format-ascend
    descend-prefix->format-proc
    scm-format
    scm-format-default-config
    scm-format-port
    sph-lang-scm-format-description)
  (import
    (ice-9 streams)
    (rnrs base)
    (rnrs bytevectors)
    (sph)
    (sph hashtable)
    (sph lang scm-format format)
    (sph lang scm-format transform)
    (sph system reader)
    (only (guile)
      inf
      object->string
      write
      array-type
      string-join)
    (only (sph list) map-integers)
    (only (sph string)
      any->string
      any->string-write*
      string-multiply)
    (only (sph tree) tree-transform-with-state))

  (define sph-lang-scm-format-description "format scheme code")
  (define default-format-ascend format-application)

  (define-as scm-format-default-config symbol-hashtable
    format
    (symbol-hashtable indent-string (string-multiply " " 2)
      max-chars-per-line 100
      max-exprs-per-line-start 3
      max-exprs-per-line-middle 2
      max-exprs-per-line-end (inf)
      use-read-syntax-quote #f
      multiple-leading-parenthesis-spacing #t
      toplevel-vertical-spacing 1 toplevel-vertical-spacing-oneline 0)
    transform
    (symbol-hashtable sort-export #t
      sort-import #t sort-definitions #f separate-unexported-definitions #f))

  (define-as descend-prefix->format-proc symbol-hashtable
    semicolon-comment format-semicolon-comment
    scsh-block-comment format-scsh-block-comment
    hash-bang format-hash-bang
    range-comment format-range-comment
    lambda format-lambda
    quote format-quote
    unquote format-unquote
    syntax format-syntax
    unsyntax format-unsyntax
    quasiquote format-quasiquote
    quasisyntax format-quasisyntax
    let format-let
    let-macro format-list-assoc
    define format-lambda
    define-syntax-rule format-lambda
    define-syntax-case format-lambda
    define* format-lambda library format-library define-test-module format-test-module)

  (define ascend-prefix->format-proc (hashtable))

  (define (config-add-defaults c) "r6rs-hashtable -> r6rs-hashtable"
    (let
      ( (config-format (hashtable-ref c (q format)))
        (config-transform (hashtable-ref c (q transform)))
        (default-config-transform (hashtable-ref scm-format-default-config (q transform)))
        (default-config-format (hashtable-ref scm-format-default-config (q format))))
      (hashtable (q format)
        (if config-format (hashtable-merge default-config-format config-format)
          default-config-format)
        (q transform)
        (if config-transform (hashtable-merge default-config-transform config-transform)
          default-config-transform))))

  (define (format-sequence a a-length ref current-indent config config-format)
    (first
      (scm-format-list->string (map-integers a-length (l (index) (ref a index))) current-indent
        config)))

  (define (format-non-list a current-indent config config-format)
    (cond ((string? a) (format-string a config-format current-indent))
      ( (vector? a)
        (string-append "#"
          (format-sequence a (vector-length a) vector-ref current-indent config config-format)))
      ((pair? a) (any->string-write* a)) (else (any->string-write* a))))

  (define (scm-format-list->string a nesting-depth config)
    (let (config-format (hashtable-ref config (q format)))
      (tree-transform-with-state a
        (l (expr recurse current-indent)
          (let (format-proc (hashtable-ref descend-prefix->format-proc (first expr)))
            (if format-proc
              (apply
                (l (r continue?)
                  (list r continue? (if continue? (+ 1 current-indent) current-indent)))
                (format-proc expr recurse config-format (+ 1 current-indent)))
              (list #f #t (+ 1 current-indent)))))
        (l (expr current-indent)
          (list
            ( (hashtable-ref ascend-prefix->format-proc (first expr) default-format-ascend) expr
              config-format current-indent)
            (- current-indent 1)))
        (l (expr current-indent)
          (list (format-non-list expr current-indent config config-format) current-indent))
        nesting-depth)))

  (define (primitive-scm-format a current-indent config)
    "list integer:current-indent r6rs-hashtable:config -> string"
    (apply
      (l (r is-library)
        (string-join-with-vertical-spacing r ""
          (hashtables-ref config (q format) (q toplevel-vertical-spacing))
          (hashtables-ref config (q format) (q toplevel-vertical-spacing-oneline))))
      (list
        (map (l (e) (first (scm-format-list->string e current-indent config)))
          (scm-format-transform-tree a (hashtable-ref config (q transform))))
        (any is-library? a))))

  (define* (scm-format a #:optional (current-indent 0) config) "any -> string"
    (primitive-scm-format (list a) 0
      (if config (config-add-defaults config) scm-format-default-config)))

  (define* (scm-format-port a #:optional config) "port [r6rs-hashtable] -> string"
    (let (config (if config (config-add-defaults config) scm-format-default-config))
      (primitive-scm-format (stream->list (port->stream a read-for-formatting)) 0 config))))
