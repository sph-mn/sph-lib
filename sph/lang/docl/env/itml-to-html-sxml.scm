(library (sph lang docl env itml-to-html-sxml)
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
    section
    section*
    sort
    sxml
    sxml-indent
    table
    ul)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph lang docl env)
    (sph lang indent-syntax)
    (only (guile)
      make-list
      compose
      string<
      string-suffix?
      string-split
      string-join)
    (only (sph list) simplify-list map-span)
    (only (sph one) string->datum)
    (only (sph string) string-multiply any->string))

  (define html-headings (q #(h1 h1 h2 h3 h4 h5 h6)))

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
            (pair
              (if (symbol? (first e)) e
                (pair (q div) (append (create-sxml-indent (max 0 (- indent-depth 1))) e (ql (br)))))
              r))
          (pair (list (q span) (create-sxml-indent (max 0 (- indent-depth 1))) e (ql br)) r)))
      (list) a))

  (define (list-sort-as-string string-less? a)
    (list-sort (l (a b) (string-less? (any->string a) (any->string b))) a))

  (define (sort a) (as-list (list-sort-as-string string< a)))
  (define (p a) (pair (q p) (add-spaces a)))

  (define (section* level title content . attributes)
    (pair (q span)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (h* level (list (create-sxml-indent (max 0 (- level 2))) title))
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content)
                (map-span (compose not list?) (l non-lists (pair (q div) non-lists)) content)))
            (list (q div) content))))))

  (define-syntax-rule (sxml a) (list (quasiquote a)))
  (define (h* number content) (pair (vector-ref html-headings (min 6 number)) content))

  (define (escape a indent-depth)
    (list (q pre)
      (string-join (map (l (e) (if (list? e) (prefix-tree->indent-tree-string (list e)) e)) a) "\n")))

  (define (escape-with-indent a indent-depth)
    (let
      ( (indent (string-multiply " " (* 2 (max 0 (- indent-depth 1)))))
        (lines
          (fold
            (l (e r)
              (if (list? e)
                (append (string-split (prefix-tree->indent-tree-string (list e)) #\newline) r)
                (pair e r)))
            (list) a)))
      (list (q pre) (string-join (map (l (e) (string-append indent e)) lines) "\n"))))

  (define (section title . content) (section* (or (docl-env-ref (q indent-depth)) 1) title content))
  (define (h . content) (h* (or (docl-env-ref (q indent-depth)) 1) content))
  (define (ul a . rest) (pair (q ul) (map (l (e) (list (q li) e)) a)))
  (define (ol a . rest) (pair (q ol) (map (l (e) (list (q li) e)) a)))

  (define (table a . rest)
    (pair (q table)
      (map (l (e) (pair (q tr) (if (list? e) (map (l (e) (list (q td) e)) e) (list e)))) a))))