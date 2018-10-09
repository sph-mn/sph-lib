(library (sph lang itml read)
  (export
    path->itml-parsed
    port->itml-parsed
    sph-lang-itml-read-description
    string->itml-parsed)
  (import
    (ice-9 peg)
    (sph)
    (sph conditional)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph lang scheme)
    (sph list)
    (sph string)
    (sph tree)
    (only (guile)
      open-input-string
      call-with-input-file
      string-trim-right
      identity
      const
      read
      string-drop-right)
    (only (srfi srfi-1) remove split-at))

  (define sph-lang-itml-read-description
    "parse itml.
     # escapes
     \\:
     \\#
     \\##
     \\###
     \\\\")

  (define-peg-pattern expression-prefix-scm none (and "#" (not-followed-by "#")))
  (define-peg-pattern expression-prefix-text none (and "##" (not-followed-by "#")))
  (define-peg-pattern expression-prefix-descend none (and "###" (not-followed-by "#")))
  (define-peg-pattern association-infix all ": ")
  (define-peg-pattern escaped-backslash-body body (and ignored-backslash "\\"))
  (define-peg-pattern ignored-backslash none "\\")
  (define-peg-pattern ignored-association-infix none ": ")
  (define-peg-pattern ignored-space none " ")
  (define-peg-pattern ignored-opening-parenthesis none "(")
  (define-peg-pattern ignored-closing-parenthesis none ")")
  (define-peg-pattern escaped-association-infix body (and ignored-backslash ": "))

  (define-peg-pattern escaped-expression-prefix-inner body
    (and ignored-backslash
      (or (and "###" (not-followed-by "#")) (and "##" (not-followed-by "#"))
        (and "#" (not-followed-by "#")))))

  (define-peg-pattern escaped-expression-prefix all escaped-expression-prefix-inner)
  (define-peg-pattern escaped-expression-prefix-body body escaped-expression-prefix-inner)

  (define-peg-pattern association-left-char body
    (or (range #\a #\z) (range #\0 #\9)
      " " "-" "\"" "?" "*" "!" "#" ">" "<" "." "+" "_" "/" "$" "%" "&" "*"))

  (define-peg-pattern identifier all
    (+
      (or (range #\a #\z) (range #\0 #\9)
        "-" "?" "*" "!" "#" ">" "<" "." "+" "_" "/" "$" "%" "&" "*")))

  (define-peg-pattern inline-scm-expression-inner body
    (and "(" (* (or inline-scm-expression-inner (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern inline-scm-expression all
    (and expression-prefix-scm "("
      (* (or inline-scm-expression-inner (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern indent-scm-expression all
    (and expression-prefix-scm identifier
      (not-followed-by ignored-association-infix) (? ignored-space) (* peg-any)))

  (define-peg-pattern line-scm-expression all
    (and expression-prefix-scm identifier ignored-association-infix (* peg-any)))

  (define-peg-pattern inline-text-expression-inner all
    ; the inner expressionessions are parsed to close the expressions at the appropriate brackets
    (and (? ignored-space) ignored-opening-parenthesis
      (*
        (or inline-text-expression-inner escaped-backslash-body
          line-text-expression inline-scm-expression
          line-scm-expression association inline-text-expression (and (not-followed-by ")") peg-any)))
      ignored-closing-parenthesis))

  (define-peg-pattern inline-text-expression all
    (and expression-prefix-text ignored-opening-parenthesis
      (*
        (or inline-text-expression-inner
          (and (not-followed-by ")") (or (and identifier ignored-space) peg-any))))
      ignored-closing-parenthesis))

  (define-peg-pattern line-text-expression all
    (and expression-prefix-text identifier
      ignored-association-infix
      (* (or inline-scm-expression line-scm-expression line-text-expression association peg-any))))

  (define-peg-pattern indent-text-expression all
    (and expression-prefix-text identifier (? (and ignored-space (* peg-any)))))

  (define-peg-pattern indent-descend-expression all
    (and expression-prefix-descend identifier (? (and ignored-space (* peg-any)))))

  (define-peg-pattern association all
    (and
      (+
        (and (not-followed-by (and " " ignored-association-infix))
          (or escaped-backslash-body escaped-expression-prefix-body
            inline-text-expression inline-scm-expression
            escaped-association-infix association-left-char)))
      association-infix
      (*
        (or escaped-backslash-body escaped-expression-prefix-body
          line-scm-expression inline-scm-expression
          line-text-expression inline-text-expression escaped-association-infix peg-any))))

  (define-peg-pattern line all
    (*
      (or escaped-backslash-body escaped-expression-prefix
        inline-text-expression line-text-expression
        inline-scm-expression line-scm-expression association escaped-association-infix peg-any)))

  (define-peg-pattern ascend-prefix-expression body (or indent-text-expression (* peg-any)))
  (define-peg-pattern descend-expression body (or indent-descend-expression indent-scm-expression))

  (define (match-prefix-expression prefix content)
    (and-let* ((match (if-pass (match-pattern descend-expression prefix) peg:tree)))
      (append (any->list match) content)))

  (define (descend a re-descend)
    (let (prefix (first a))
      (if (string? prefix)
        (let (match (match-prefix-expression prefix (tail a)))
          (if match (list match #f) (list #f #t)))
        (list #f #t))))

  (define (ascend a)
    (let (prefix (first a))
      (if (string? prefix)
        (let (match (match-pattern ascend-prefix-expression prefix))
          (if match
            (let (match (peg:tree match))
              ; extra list wrap for consistency with descend-expression
              (if (list? match) (list (append match (tail a))) (pair match (tail a))))
            a))
        a)))

  (define (terminal a) "string -> any"
    (let (b (peg:tree (match-pattern line a)))
      (if (list? b) (if (= 2 (length b)) (first (tail b)) b) (if (symbol? b) (q line-empty) b))))

  (define (read-scm-expression a) (string->datum (any->string-display a) read))
  (define (string->datums a) "string -> list" (string->datum (parenthesise a) read))
  (define (split-at& a index c) (call-with-values (nullary (split-at a index)) c))

  (define-as prefix->handler-ht ht-create-symbol-q
    inline-scm-expression
    (l (a) (pair (q inline-scm-expression) (simplify-list (read-scm-expression a))))
    line-scm-expression (l (a) (pair (q line-scm-expression) (read-scm-expression a)))
    indent-scm-expression (l (a) (pair (q indent-scm-expression) (read-scm-expression a)))
    line-text-expression (l (a) (pair (q line-text-expression) a))
    identifier first
    escaped-expression-prefix first
    inline-text-expression-inner identity
    association
    (l (a)
      "-> (symbol keyword content ...)
      remove association infix. note: association keyword can be a list.
      if the association infix would not be parsed as a list it would be merged with the text or alternatively left out by the peg-parser"
      (let* ((content (tail (tail a))) (content-prefix (first content)))
        (pairs (q association) (first a)
          (if
            (and (list? content-prefix)
              (not (or (null? content-prefix) (symbol? (first content-prefix)))))
            content-prefix content)))))

  (define finalise-tree
    (let
      (finalise-expression
        (l (a) "list -> any" (let (p (ht-ref prefix->handler-ht (first a))) (if p (p (tail a)) a))))
      (l (a) "list -> list" (finalise-expression (tree-map-lists finalise-expression a)))))

  (define (top-level-map a) #t)

  (define (port->itml-parsed a)
    "port -> list
     reads an itml string from port, parses it and returns the abstract syntax tree"
    ; parse the indent structure first separately.
    ; the top-level is not an itml-expression but a list of itml-expressions
    (let (a (denoted-tree->prefix-tree (read-indent-tree->denoted-tree a 2)))
      (map
        (l (a)
          (let (b (finalise-tree (tree-transform (any->list a) descend ascend terminal)))
            (if (null? (tail b)) (first b) b)))
        a)))

  (define (path->itml-parsed a)
    "string -> list
     like port->itml-parsed but takes a path to a file to read from"
    (call-with-input-file a port->itml-parsed))

  (define (string->itml-parsed a)
    "string -> list
     like port->itml-parsed but takes a string to parse"
    (port->itml-parsed (open-input-string a))))
