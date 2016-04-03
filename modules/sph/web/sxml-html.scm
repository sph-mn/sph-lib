(library (sph web sxml-html)
  (export
    sxml-html-alist->options
    sxml-html-heading
    sxml-html-hyperlink
    sxml-html-include-script
    sxml-html-include-style
    sxml-html-indent
    sxml-html-indent-create
    sxml-html-list->list
    sxml-html-list->table
    sxml-html-section
    sxml-html-text->sxml)
  (import
    (rnrs base)
    (sph)
    (only (guile) string-split make-list)
    (only (sph list) interleave pair->list)
    (only (sph string) any->string))

  (define html-headings (q #(h1 h2 h3 h4 h5 h6)))

  (define (sxml-html-heading nesting-depth . content)
    (pair (vector-ref html-headings (min 5 nesting-depth)) content))

  (define (sxml-html-section nesting-depth title content . attributes)
    "integer sxml sxml (string/symbol string/symbol) ... -> sxml"
    (pair (q section)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (sxml-html-heading nesting-depth title)
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content) (list (pair (q div) content))))
            (list (list (q div) content)))))))

  (define sxml-html-indent (ql (*ENTITY* "#160") (*ENTITY* "#160")))

  (define (sxml-html-indent-create indent-level)
    (apply append (make-list indent-level sxml-html-indent)))

  (define (sxml-html-text->sxml a) (interleave (string-split a #\newline) (q (br))))

  (define* (sxml-html-include-script path #:optional is-async) "string boolean -> sxml"
    (qq
      (script
        (@ (src (unquote path)) (unquote-splicing (if is-async (list (ql async async)) (list)))) "")))

  (define (sxml-html-include-style path) "string -> sxml"
    (qq (link (@ (rel "stylesheet") (type "text/css") (href (unquote path))))))

  (define* (sxml-html-hyperlink name #:optional (value name))
    (qq (a (@ (href (unquote value))) (unquote name))))

  (define (sxml-html-alist->options a) "((content . value/false) ...) -> list"
    (map
      (l (e)
        (if (pair? e) (qq (option (@ (value (unquote (tail e)))) (unquote (first e))))
          (list (q option) e)))
      a))

  (define* (sxml-html-list->list a #:optional ordered?) "list boolean -> sxml"
    (pair (if ordered? (q ol) (q ul))
      (map (l (e) (list (q li) (if (list? e) (sxml-html-list->list e) (any->string e)))) a)))

  (define (sxml-html-list->table a) "((sxml ...) ...) -> sxml"
    (pair (q table) (map (l (e) (pair (q tr) (map (l (e) (list (q td) e)) e))) a))))
