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
    (only (sph list)
      simplify-list
      map-span
      interleave)
    (only (sph one) string->datum)
    (only (sph string)
      string-multiply
      any->string
      string-slice-at-words))

  ;the html is supposed to look almost the same in browsers that support css and text browsers that do not.
  ;all the "indent" handling is only done because current text browsers do not support any css yet.

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