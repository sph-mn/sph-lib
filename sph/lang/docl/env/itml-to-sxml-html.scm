(library (sph lang docl env itml-to-sxml-html)
  (export
    add-paragraphs-and-indent
    add-spaces
    as-list
    create-sxml-indent
    escape
    escape-with-indent
    h
    h*
    ol
    p
    scm*
    section
    section*
    sort
    sxml
    a
    assign
    bttl-class-prefix
    sxml-indent
    table
    text->sxml
    ul)
  (import
    (ice-9 receive)
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph lang docl env)
    (sph web sxml-html)
    (sph lang indent-syntax)
    (only (guile)
      cons*
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

  ;the assign syntax is for applying css styles inline, while css inclusion is handled automatically

  (define-syntax-rule (a arg ...)
    ;alias for assign
    (assign arg ...))

  (define temporary-class
    (let (count 0)
      (lambda ()
        "create a temporary style class. the classes are numbered,
        	each new one counted upwards as long as the process runs."
        (if (> count 9223372036854775806) (set! count 0) (set! count (+ 1 count)))
        (string-append bttl-class-prefix (number->string count 32)))))

  (define bttl-class-prefix "bttl-")

  (define (style-value arg)
    "string\\number\\symbol -> string
    handle values for a style property"
    (if (string? arg) arg
      (if (number? arg) (number->string arg)
        (if (symbol? arg) (symbol->string arg) (throw (q wrong-type-for-argument) arg)))))

  (define (style-property arg)
    "pair -> string
    handle one property to value expression"
    (let
      ( (property-name
          (let ((property-name (first arg)))
            (if (string? property-name) property-name (symbol->string property-name))))
        (value (tail arg)))
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
                (list (cons temp-class class) (assign-stylesheet temp-class style)))))))))

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
                (list (cons temp-class class) (assign-stylesheet temp-class style)))))))))

  (define-syntax-rule (assign class\style ... text)
    ;assign classes and css styles to the contained text
    (primitive-assign (quote (class\style ...)) text))

  (define (text->sxml arg)
    "string -> sxml
    convert newlines in string to (br) and result in an sxml expression"
    (let (arg (string-split arg #\newline))
      (if (length-eq-one? arg) (first arg)
        (tail
          (fold-right (l (ele prev) (cons (q (br)) (cons ele prev))) (list (first arg)) (tail arg))))))

  (define (scm* arg)
    "s-expr -> sxml
    evaluates a scheme expression and formats the result for use as html
    vector -> table
    list -> unordered list
    string -> text"
    (list
      (if (string? arg) (text->sxml arg)
        (if (list? arg) (sxml-html-list->list arg) (any->string arg)))))

  (define-syntax-rule (sxml expr)
    ;return the enclosed expression as sxml.
    ;the sxml is quasiquoted and so can contain dynamically computed expressions
    (list (qq expr)))

  (define html-headings (q #(h1 h2 h3 h4 h5 h6)))
  (define text-column-max-length 160)

  (define (string-list-add-indent+break a indent)
    (interleave (map (l (e) (list indent e)) a) (ql br)))

  (define (text-wrap-with-indent a indent)
    (let (a-length (string-length a))
      (if (> a-length text-column-max-length)
        (pair (q p)
          (string-list-add-indent+break (string-slice-at-words a text-column-max-length) indent))
        (if (= 0 a-length) a (list indent a (ql br))))))

  (define (text-columns-wrap-with-indent a indent-depth)
    "this is done for supporting indent alignment for multiple lines in text-browsers.
    text browser should support some css properties for using a box-model because this way conflicts
    with different font-sizes and screen-filling preferences for example"
    (let (indent (create-sxml-indent indent-depth))
      (reverse (fold (l (e r) (if (string? e) (text-wrap-with-indent e indent) e)) (list) a))))

  (define (add-spaces a)
    "inserts a space before non-list elements (strings, numbers, etc) except the first and splices lists of expressions."
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

  (define sxml-indent (ql (*ENTITY* "#160") (*ENTITY* "#160")))
  (define (create-sxml-indent indent-depth) (apply append (make-list indent-depth sxml-indent)))

  (define (add-paragraphs-and-indent a indent-depth)
    "removes empty list elements, unneccessary nesting, and wraps lists that do not have a symbol as the first element
    with <p>"
    (fold-right
      (l (e r)
        (if (list? e)
          (if (null? e) r
            (pair (if (symbol? (first e)) e (list (create-sxml-indent indent-depth) e (ql br))) r))
          (pair
            (if (string? e) (text-wrap-with-indent e (create-sxml-indent indent-depth))
              (list (create-sxml-indent indent-depth) e (ql br)))
            r)))
      (list) a))

  (define (list-sort-as-string string-less? a)
    (list-sort (l (a b) (string-less? (any->string a) (any->string b))) a))

  (define (sort a) (as-list (list-sort-as-string string< a)))
  (define (p a) (pair (q p) (add-spaces a)))

  (define (section* level title content . attributes)
    (pair (q section)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (h* level (list (create-sxml-indent level) title))
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content)
                (map-span (compose not list?) (l non-lists (pair (q div) non-lists)) content)))
            (list (q div) content))))))

  (define-syntax-rule (sxml a) (list (quasiquote a)))
  (define (h* number content) (pair (vector-ref html-headings (min 5 number)) content))

  (define (escape a indent-depth)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n")))

  (define (escape-with-indent a indent-depth)
    (let
      ( (indent (string-multiply "  " indent-depth))
        (lines
          (reverse
            (fold
              (l (e r)
                (if (list? e)
                  (append
                    (reverse (string-split (prefix-tree->indent-tree-string (list e)) #\newline)) r)
                  (pair e r)))
              (list) a))))
      (list (q pre) (string-append indent (string-join lines (string-append "\n" indent))))))

  (define (section title . content) (section* (or (docl-env-ref (q indent-depth)) 0) title content))
  (define (h . content) (h* (or (docl-env-ref (q indent-depth)) 0) content))
  (define (ul a . rest) (pair (q ul) (map (l (e) (list (q li) e)) a)))
  (define (ol a . rest) (pair (q ol) (map (l (e) (list (q li) e)) a)))

  (define (table a . rest)
    (pair (q table)
      (map (l (e) (pair (q tr) (if (list? e) (map (l (e) (list (q td) e)) e) (list e)))) a))))