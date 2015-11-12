(library (sph lang docl env itml-to-sxml-html)
  (export
    a
    add-spaces
    as-list
    assign
    bttl-class-prefix
    current-nesting-level
    escape
    h
    h*
    ol
    p
    scm*
    section
    section*
    sort
    sxml
    table
    text->sxml
    ul)
  (import
    (ice-9 receive)
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph lang docl env)
    (sph lang indent-syntax)
    (sph web sxml-html)
    (only (guile)
      string-split
      string-join
      make-list
      compose
      string<
      string-suffix?
      string-split
      string-join)
    (only (sph list)
      length-eq-one?
      list-prefix?
      map-successive
      simplify-list
      map-span
      interleave)
    (only (sph one) string->datum)
    (only (sph string)
      string-multiply
      any->string
      string-slice-at-words)
    (only (srfi srfi-1)
      append!
      delete-duplicates!
      fold-right
      map!
      partition!
      reverse!))

  (define-syntax-rule (current-nesting-level) (or (docl-env-ref (q nesting-level)) 0))
  ;the assign syntax is for applying css styles inline, while css inclusion is handled automatically

  (define-syntax-rule (a a ...)
    ;alias for assign
    (assign a ...))

  (define temporary-class
    (let (count 0)
      (lambda ()
        "create a temporary style class. the classes are numbered,
        each new one counted upwards as long as the process runs."
        (if (> count 9223372036854775806) (set! count 0) (set! count (+ 1 count)))
        (string-append bttl-class-prefix (number->string count 32)))))

  (define bttl-class-prefix "bttl-")

  (define (style-value a)
    "string\\number\\symbol -> string
    handle values for a style property"
    (if (string? a) a
      (if (number? a) (number->string a)
        (if (symbol? a) (symbol->string a) (throw (q wrong-type-for-argument) a)))))

  (define (style-property a)
    "pair -> string
    handle one property to value expression"
    (let
      ( (property-name
          (let ((property-name (first a)))
            (if (string? property-name) property-name (symbol->string property-name))))
        (value (tail a)))
      (string-append property-name ":"
        (if (list? value) (string-join (map style-value value) " ") (style-value value)))))

  (define (assign-stylesheet class styles)
    "list -> string
    create a stylesheet string from the result of handle-assign"
    (string-append "." class "{" (string-join (map style-property styles) ";") "}"))

  (define (primitive-assign class\style text)
    "list string -> text style
    procedure for the assign syntax"
    (call-with-values (l () (partition! symbol? class\style))
      (l (class style)
        (apply
          (l (class . style)
            (list
              (qq
                (span (@ (class (unquote (string-join class " "))))
                  (unquote (text->sxml (any->string text)))))
              style))
          (let (class (map! symbol->string class))
            (if (null? style) (list class)
              (let (temp-class (temporary-class))
                (list (pair temp-class class) (assign-stylesheet temp-class style)))))))))

  (define (assign-stylesheet class styles)
    "list -> string
    create a stylesheet string from the result of handle-assign"
    (string-append "." class "{" (string-join (map style-property styles) ";") "}"))

  (define (primitive-assign class\style text)
    "list string -> text style
    procedure for the assign syntax"
    (call-with-values (l () (partition! symbol? class\style))
      (l (class style)
        (apply
          (l (class . style)
            (list
              (qq
                (span (@ (class (unquote (string-join class " "))))
                  (unquote (text->sxml (any->string text)))))
              style))
          (let (class (map! symbol->string class))
            (if (null? style) (list class)
              (let (temp-class (temporary-class))
                (list (pair temp-class class) (assign-stylesheet temp-class style)))))))))

  (define-syntax-rule (assign class\style ... text)
    ;assign classes and css styles to the contained text
    (primitive-assign (quote (class\style ...)) text))

  (define (text->sxml a)
    "string -> sxml
    convert newlines in string to (br) and result in an sxml expression"
    (let (a (string-split a #\newline))
      (if (length-eq-one? a) (first a)
        (tail (fold-right (l (e r) (pair (ql br) (pair e r))) (list (first a)) (tail a))))))

  (define (scm* a)
    "s-expr -> sxml
    evaluates a scheme expression and formats the result for use as html
    vector -> table
    list -> unordered list
    string -> text"
    (list (if (string? a) (text->sxml a) (if (list? a) (sxml-html-list->list a) (any->string a)))))

  (define-syntax-rule (sxml expr)
    ;return the enclosed expression as sxml.
    ;the sxml is quasiquoted and so can contain dynamically computed expressions
    (list (qq expr)))

  (define html-headings (q #(h1 h2 h3 h4 h5 h6)))
  (define text-column-max-length 120)

  (define (add-spaces a)
    "list -> list
    inserts a space before non-list elements (strings, numbers, etc) except the first and splices lists of expressions."
    (fold-right
      (l (e r)
        (if (list? e) (if (null? e) (pair e r) (if (symbol? (first e)) (pair e r) (append e r)))
          (if (null? r) (pair e r)
            (if (and (string? e) (string-suffix? " " e)) (pair e r) (pairs e " " r)))))
      (list) a))

  (define (as-list a) "creates an unordered list from lines"
    (if (= (length a) 1) (first a)
      (pair (q ul)
        (fold-right
          (l (e r)
            (if (list? e)
              (if (null? e) (pair (pair (q li) e) r)
                (if (symbol? (first e)) (pair (list (q li) e) r) (pair (pair (q li) e) r)))
              (pair (list (q li) e) r)))
          (list) a))))

  (define (list-sort-as-string string-less? a)
    (list-sort (l (a b) (string-less? (any->string a) (any->string b))) a))

  (define (sort a) (as-list (list-sort-as-string string< a)))
  (define (p a) (pair (q p) (add-spaces a)))

  (define (section* level title content . attributes)
    (pair (q section)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (h* level title)
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content) (list (pair (q div) content))))
            (list (list (q div) content)))))))

  (define-syntax-rule (sxml a) (list (quasiquote a)))
  (define (h* number . content) (pair (vector-ref html-headings (min 5 number)) content))

  (define (escape a nesting-level)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n")))

  (define (section title . content) (section* (current-nesting-level) title content))
  (define (h . content) (apply h* (current-nesting-level) content))
  (define (ul a . rest) (pair (q ul) (map (l (e) (list (q li) e)) a)))
  (define (ol a . rest) (pair (q ol) (map (l (e) (list (q li) e)) a)))

  (define (table a . rest)
    (pair (q table)
      (map (l (e) (pair (q tr) (if (list? e) (map (l (e) (list (q td) e)) e) (list e)))) a))))
