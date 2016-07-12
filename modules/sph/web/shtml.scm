(library (sph web shtml)
  (export
    shtml-alist->options
    shtml-heading
    shtml-hyperlink
    shtml-include-css
    shtml-include-javascript
    shtml-indent
    shtml-indent-create
    shtml-list->list
    shtml-list->table
    shtml-section
    shtml-text->sxml)
  (import
    (rnrs base)
    (sph)
    (only (guile) string-split make-list)
    (only (sph list) interleave pair->list)
    (only (sph string) any->string))

  (define html-headings (q #(h1 h2 h3 h4 h5 h6)))

  (define (shtml-heading nesting-depth . content)
    (pair (vector-ref html-headings (min 5 nesting-depth)) content))

  (define (shtml-section nesting-depth title content . attributes)
    "integer sxml sxml (string/symbol string/symbol) ... -> sxml"
    (pair (q section)
      (append (if (null? attributes) attributes (list (pair (q @) attributes)))
        (pair (shtml-heading nesting-depth title)
          (if (list? content)
            (if (null? content) (list)
              (if (symbol? (first content)) (list content) (list (pair (q div) content))))
            (list (list (q div) content)))))))

  (define shtml-indent (ql (*ENTITY* "#160") (*ENTITY* "#160")))
  (define (shtml-indent-create indent-level) (apply append (make-list indent-level shtml-indent)))
  (define (shtml-text->sxml a) (interleave (string-split a #\newline) (q (br))))

  (define* (shtml-include-javascript path #:optional is-async) "string boolean -> sxml"
    (qq
      (script
        (@ (src (unquote path)) (unquote-splicing (if is-async (list (ql async async)) (list)))) "")))

  (define (shtml-include-css path) "string -> sxml"
    (qq (link (@ (rel "stylesheet") (type "text/css") (href (unquote path))))))

  (define* (shtml-hyperlink name #:optional (value name))
    (qq (a (@ (href (unquote value))) (unquote name))))

  (define (shtml-alist->options a) "((content . string:value/false)/string ...) -> list"
    (map
      (l (e)
        (if (pair? e) (qq (option (@ (value (unquote (tail e)))) (unquote (first e))))
          (list (q option) e)))
      a))

  (define* (shtml-list->list a #:optional ordered?) "list boolean -> sxml"
    (pair (if ordered? (q ol) (q ul))
      (map (l (e) (list (q li) (if (list? e) (shtml-list->list e) (any->string e)))) a)))

  (define (shtml-list->table a) "((sxml ...) ...) -> sxml"
    (pair (q table) (map (l (e) (pair (q tr) (map (l (e) (list (q td) e)) e))) a))))
