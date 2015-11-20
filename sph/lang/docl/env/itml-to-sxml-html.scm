(library (sph lang docl env itml-to-sxml-html)
  (export
    add-spaces
    as-list
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
