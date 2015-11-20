(library (sph lang parser itml)
  (export
    path->itml-parsed
    port->itml-parsed
    string->itml-parsed)
  (import
    (ice-9 peg)
    (rnrs base)
    (sph)
    (sph lang indent-syntax)
    (sph tree)
    (only (guile)
      open-input-string
      call-with-input-file
      string-trim-right
      string-drop-right)
    (only (sph conditional) pass-if)
    (only (srfi srfi-1) remove))

  (define-peg-pattern double-backslash all "\\\\")
  (define-peg-pattern unescaped-backslash none (and "\\" (not-followed-by "\\")))
  (define-peg-pattern association-infix all ": ")
  (define-peg-pattern ignored-dot none ".")
  (define-peg-pattern ignored-space none " ")
  (define-peg-pattern ignored-opening-parenthesis none "(")
  (define-peg-pattern ignored-closing-parenthesis none ")")

  (define-peg-pattern invalid-association-infix body
    (and (or unescaped-backslash " ") association-infix))

  (define-peg-pattern association-left-char body
    (or (range #\a #\z) (range #\0 #\9) " " "-" "\"" "?" "*" "!" "#" ">" "<" "." "+" "_" "/"))

  (define-peg-pattern identifier all
    (+ (or (range #\a #\z) (range #\0 #\9) "-" "?" "*" "!" "#" ">" "<" "." "+" "_" "/")))

  (define-peg-pattern inline-expr-inner all
    (and ignored-opening-parenthesis
      (*
        (or inline-expr-inner double-backslash
          line-scm-expr line-expr
          association inline-scm-expr inline-expr (and (not-followed-by ")") peg-any)))
      ignored-closing-parenthesis))

  (define-peg-pattern inline-expr all
    (and unescaped-backslash ignored-opening-parenthesis
      (*
        (or inline-expr-inner
          (and (not-followed-by ")") (or (and identifier ignored-space) peg-any))))
      ignored-closing-parenthesis))

  (define-peg-pattern inline-scm-expr-inner body
    (and "(" (* (or inline-scm-expr-inner (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern inline-scm-expr all
    (and unescaped-backslash ignored-dot
      "(" (* (or inline-scm-expr-inner (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern ignored-association-infix none ": ")

  (define-peg-pattern line-expr all
    (and unescaped-backslash identifier
      ignored-association-infix (* (or inline-scm-expr line-scm-expr line-expr association peg-any))))

  (define-peg-pattern line-scm-expr all
    (and unescaped-backslash ignored-dot identifier ignored-association-infix (* peg-any)))

  (define-peg-pattern indent-scm-expr all
    (and unescaped-backslash ignored-dot identifier (not-followed-by association-infix) (* peg-any)))

  (define-peg-pattern indent-expr all
    (and unescaped-backslash (not-followed-by ".") identifier (? (and ignored-space (* peg-any)))))

  (define-peg-pattern indent-descend-expr all
    (and unescaped-backslash (ignore "#") identifier (? (and ignored-space (* peg-any)))))

  (define-peg-pattern escaped-association-infix body (and unescaped-backslash ": "))

  (define-peg-pattern association all
    (and
      (+
        (and (not-followed-by (or (and " " association-infix) escaped-association-infix))
          association-left-char))
      association-infix
      (*
        (or double-backslash line-scm-expr
          inline-scm-expr line-expr escaped-association-infix peg-any))))

  (define-peg-pattern line all
    (*
      (or double-backslash inline-scm-expr
        line-scm-expr line-expr escaped-association-infix association inline-expr peg-any)))

  (define-peg-pattern ascend-prefix-expr body (or indent-expr (* peg-any)))
  (define-peg-pattern descend-expr body (or indent-scm-expr indent-descend-expr))

  (define (match-prefix-expr prefix content)
    (and-let* ((match (pass-if (search-for-pattern descend-expr prefix) peg:tree)))
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
              (if (list? match) (append match (tail a)) (pair match (tail a))))
            a))
        a)))

  (define (terminal a) "string -> any"
    (let (e (peg:tree (match-pattern line a)))
      (if (list? e) (if (= 2 (length e)) (first (tail e)) e) (if (symbol? e) (q line-empty) e))))

  (define (append-double-backslash a) "list -> list"
    (fold
      (l (e r)
        (if (and (list? e) (not (null? e)) (eqv? (q double-backslash) (first e)))
          (if (null? r) (pair "\\" r)
            (if (string? (first r)) (pair (string-append "\\" (first r)) (tail r)) (pair "\\" r)))
          (pair e r)))
      (list) (reverse a)))

  (define (splice-non-symbol-prefix-lists a) "list -> list"
    (if (null? a) a
      (let (e (first a))
        ( (if (list? e)
            (if (null? e) append
              (if (symbol? (first e)) pair (l (e a) (append (splice-non-symbol-prefix-lists e) a))))
            pair)
          e (splice-non-symbol-prefix-lists (tail a))))))

  (define (finalise-tree a) "list -> list"
    (tree-map-lists
      (l (e)
        (let (e (append-double-backslash e))
          (case (first e) ((identifier) (first (tail e)))
            ((inline-expr-inner) (tail e)) ((association-infix) #f)
            ( (association)
              (pair (q association) (splice-non-symbol-prefix-lists (remove not (tail e)))))
            ( (inline-expr)
              (let (e-tail (tail e))
                (if (null? e-tail) e
                  (pairs (q inline-expr) (string-trim-right (first e-tail)) (tail e-tail)))))
            (else e))))
      a))

  (define (port->itml-parsed a)
    "port -> list
    reads an itml string from port, parses it and returns the abstract syntax tree"
    (let (tree (read-space-indent-tree->denoted-tree a 2))
      (if (null? tree) tree
        (finalise-tree (tree-transform (denoted-tree->prefix-tree tree) descend ascend terminal)))))

  (define (path->itml-parsed a)
    "string -> list
    like port->itml-parsed but takes a path to a file to read from"
    (call-with-input-file a port->itml-parsed))

  (define (string->itml-parsed a)
    "string -> list
    like port->itml-parsed but takes a string to parse"
    (port->itml-parsed (open-input-string a))))
