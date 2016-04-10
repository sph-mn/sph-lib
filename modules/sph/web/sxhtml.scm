(library (sph web sxhtml)
  (export
    sxhtml-alist->options
    sxhtml-heading
    sxhtml-hyperlink
    sxhtml-include-script
    sxhtml-include-style
    sxhtml-indent
    sxhtml-indent-create
    sxhtml-list->list
    sxhtml-list->table
    sxhtml-section
    sxhtml-text->sxml)
  (import
    (rnrs base)
    (sph)
    (only (guile) string-split make-list)
    (only (sph list) interleave pair->list)
    (only (sph string) any->string))

  (define html-headings (q #(h1 h2 h3 h4 h5 h6)))

  (define (sxhtml-heading nesting-depth . content)
    (pair (vector-ref html-headings (min 5 nesting-depth)) content))

  (define (sxhtml-section nesting-depth title content . attributes)
    "integer sxml sxml (string/symbol string/symbol) ... -> sxml"
    (pair (q section)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (sxhtml-heading nesting-depth title)
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content) (list (pair (q div) content))))
            (list (list (q div) content)))))))

  (define sxhtml-indent (ql (*ENTITY* "#160") (*ENTITY* "#160")))

  (define (sxhtml-indent-create indent-level)
    (apply append (make-list indent-level sxhtml-indent)))

  (define (sxhtml-text->sxml a) (interleave (string-split a #\newline) (q (br))))

  (define* (sxhtml-include-script path #:optional is-async) "string boolean -> sxml"
    (qq
      (script
        (@ (src (unquote path)) (unquote-splicing (if is-async (list (ql async async)) (list)))) "")))

  (define (sxhtml-include-style path) "string -> sxml"
    (qq (link (@ (rel "stylesheet") (type "text/css") (href (unquote path))))))

  (define* (sxhtml-hyperlink name #:optional (value name))
    (qq (a (@ (href (unquote value))) (unquote name))))

  (define (sxhtml-alist->options a) "((content . value/false) ...) -> list"
    (map
      (l (e)
        (if (pair? e) (qq (option (@ (value (unquote (tail e)))) (unquote (first e))))
          (list (q option) e)))
      a))

  (define* (sxhtml-list->list a #:optional ordered?) "list boolean -> sxml"
    (pair (if ordered? (q ol) (q ul))
      (map (l (e) (list (q li) (if (list? e) (sxhtml-list->list e) (any->string e)))) a)))

  (define (sxhtml-list->table a) "((sxml ...) ...) -> sxml"
    (pair (q table) (map (l (e) (pair (q tr) (map (l (e) (list (q td) e)) e))) a))))
