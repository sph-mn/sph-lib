(library (sph lang itml read)
  (export
    path->itml-parsed
    port->itml-parsed
    sph-lang-itml-read-description
    string->itml-parsed)
  (import
    (ice-9 peg)
    (sph)
    (sph lang indent-syntax)
    (sph tree)
    (only (guile)
      open-input-string
      call-with-input-file
      string-trim-right
      string-join
      identity
      const
      read
      string-drop-right)
    (only (sph conditional) if-pass)
    (only (sph hashtable) hashtable-ref symbol-hashtable)
    (only (sph list)
      first-or-false
      any->list
      simplify-list
      splice-last-list
      list-tail-ref)
    (only (sph one) string->datum)
    (only (sph string)
      parenthesise
      any->string
      any->string-display
      any->string-write)
    (only (srfi srfi-1) remove split-at))

  (define sph-lang-itml-read-description "itml -> syntax-tree")
  (define-peg-pattern double-backslash-body body (and ignored-backslash "\\"))
  (define-peg-pattern double-backslash all "\\\\")
  (define-peg-pattern association-infix all ": ")
  (define-peg-pattern ignored-association-infix none ": ")
  (define-peg-pattern ignored-dot none ".")
  (define-peg-pattern ignored-backslash none "\\")
  (define-peg-pattern ignored-space none " ")
  (define-peg-pattern ignored-opening-parenthesis none "(")
  (define-peg-pattern ignored-closing-parenthesis none ")")

  (define-peg-pattern association-left-char body
    (or (range #\a #\z) (range #\0 #\9) " " "-" "\"" "?" "*" "!" "#" ">" "<" "." "+" "_" "/" "$"))

  (define-peg-pattern identifier all
    (+ (or (range #\a #\z) (range #\0 #\9) "-" "?" "*" "!" "#" ">" "<" "." "+" "_" "/" "$")))

  (define-peg-pattern inline-expr-inner all
    ;the inner expressions are parsed to end the expression at the appropriate closing parenthesis
    (and (? ignored-space) ignored-opening-parenthesis
      (*
        (or inline-expr-inner double-backslash-body
          line-scm-expr line-expr
          association inline-scm-expr inline-expr (and (not-followed-by ")") peg-any)))
      ignored-closing-parenthesis))

  (define-peg-pattern inline-expr all
    (and ignored-backslash ignored-opening-parenthesis
      (*
        (or inline-expr-inner
          (and (not-followed-by ")") (or (and identifier ignored-space) peg-any))))
      ignored-closing-parenthesis))

  (define-peg-pattern inline-scm-expr-inner body
    (and "(" (* (or inline-scm-expr-inner (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern inline-scm-expr all
    (and ignored-backslash ignored-dot
      "(" (* (or inline-scm-expr-inner (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern line-expr all
    (and ignored-backslash identifier
      ignored-association-infix (* (or inline-scm-expr line-scm-expr line-expr association peg-any))))

  (define-peg-pattern line-scm-expr all
    (and ignored-backslash ignored-dot identifier ignored-association-infix (* peg-any)))

  (define-peg-pattern indent-scm-expr all
    (and ignored-backslash ignored-dot
      identifier (not-followed-by ignored-association-infix) (? ignored-space) (* peg-any)))

  (define-peg-pattern indent-expr all
    (and ignored-backslash (not-followed-by ".") identifier (? (and ignored-space (* peg-any)))))

  (define-peg-pattern indent-descend-expr all
    (and ignored-backslash (ignore "#") identifier (? (and ignored-space (* peg-any)))))

  (define-peg-pattern escaped-association-infix body (and ignored-backslash ": "))

  (define-peg-pattern association all
    (and
      (+
        (and (not-followed-by (and " " ignored-association-infix))
          (or double-backslash-body inline-scm-expr
            inline-expr escaped-association-infix association-left-char)))
      association-infix
      (*
        (or double-backslash-body line-scm-expr
          inline-scm-expr line-expr inline-expr escaped-association-infix peg-any))))

  (define-peg-pattern line all
    (*
      (or double-backslash line-scm-expr
        line-expr association inline-scm-expr escaped-association-infix inline-expr peg-any)))

  (define-peg-pattern ascend-prefix-expr body (or indent-expr (* peg-any)))
  (define-peg-pattern descend-expr body (or indent-scm-expr indent-descend-expr))

  (define (match-prefix-expr prefix content)
    (and-let* ((match (if-pass (search-for-pattern descend-expr prefix) peg:tree)))
      (append match content)))

  (define (descend a re-descend)
    (let (prefix (first a))
      (if (string? prefix)
        (let (match (match-prefix-expr prefix (tail a))) (if match (list match #f) (list #f #t)))
        (list #f #t))))

  (define (ascend a)
    (let (prefix (first a))
      (if (string? prefix)
        (let (match (search-for-pattern ascend-prefix-expr prefix))
          (if match
            (let (match (peg:tree match))
              ;extra list wrap for consistency with descend-expr
              (if (list? match) (list (append match (tail a))) (pair match (tail a))))
            a))
        a)))

  (define (terminal a) "string -> any"
    (let (e (peg:tree (match-pattern line a)))
      (if (list? e) (if (= 2 (length e)) (first (tail e)) e) (if (symbol? e) (q line-empty) e))))

  (define (read-scm-expr a) (string->datum (any->string-display a) read))
  (define (string->datums a) "string -> list" (string->datum (parenthesise a) read))
  (define (split-at& a index c) (call-with-values (nullary (split-at a index)) c))

  (define-as prefix->handler-ht symbol-hashtable
    inline-scm-expr (l (a) (pair (q inline-scm-expr) (simplify-list (read-scm-expr a))))
    line-scm-expr (l (a) (pair (q line-scm-expr) (read-scm-expr a)))
    indent-scm-expr (l (a) (pair (q indent-scm-expr) (read-scm-expr a)))
    line-expr (l (a) (pair (q line-expr) a))
    identifier first
    inline-expr-inner identity
    double-backslash (const "\\")
    ;if the association infix would not be parsed as a list it would be merged with the text or alternatively left out by the peg-parser
    association
    (l (a)
      ;-> (symbol keyword content ...)
      ;remove association infix. note: association keyword can be a list
      (let* ((content (tail (tail a))) (content-prefix (first content)))
        (pairs (q association) (first a)
          (if
            (and (list? content-prefix)
              (not (or (null? content-prefix) (symbol? (first content-prefix)))))
            content-prefix content)))))

  (define finalise-tree
    (let
      (finalise-expression
        (l (a) "list -> any"
          (let (p (hashtable-ref prefix->handler-ht (first a))) (if p (p (tail a)) a))))
      (l (a) "list -> list" (finalise-expression (tree-map-lists finalise-expression a)))))

  (define (top-level-map a) #t)

  (define (port->itml-parsed a)
    "port -> list
     reads an itml string from port, parses it and returns the abstract syntax tree"
    (let (a (denoted-tree->prefix-tree (read-space-indent-tree->denoted-tree a 2)))
      ;the top-level is not an itml-expression but a list of itml-expressions
      (map
        (l (e)
          (let (r (finalise-tree (tree-transform (any->list e) descend ascend terminal)))
            (if (null? (tail r)) (first r) r)))
        a)))

  (define (path->itml-parsed a)
    "string -> list
     like port->itml-parsed but takes a path to a file to read from"
    (call-with-input-file a port->itml-parsed))

  (define (string->itml-parsed a)
    "string -> list
     like port->itml-parsed but takes a string to parse"
    (port->itml-parsed (open-input-string a))))
