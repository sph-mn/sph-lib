(library (sph web html-sxml)
  (export
    html-sxml-alist->options
    html-sxml-hyperlink
    html-sxml-include-script
    html-sxml-include-style
    html-sxml-list->list
    html-sxml-list->table
    html-sxml-text->sxml)
  (import
    (rnrs base)
    (sph)
    (only (guile) string-split)
    (only (sph list) interleave pair->list)
    (only (sph string) any->string))

  (define (html-sxml-text->sxml arg) (interleave (string-split arg #\newline) (q (br))))

  (define* (html-sxml-include-script path #:optional is-async) "string boolean -> sxml"
    (qq
      (script
        (@ (src (unquote path)) (unquote-splicing (if is-async (list (ql async async)) (list)))) "")))

  (define (html-sxml-include-style path) "string -> sxml"
    (qq (link (@ (rel "stylesheet") (type "text/css") (href (unquote path))))))

  (define* (html-sxml-hyperlink name #:optional (value name)) (qq (a (@ (href (unquote value))) (unquote name))))

  (define (html-sxml-alist->options arg) "((content . value/false) ...) -> list"
    (map
      (l (ele)
        (if (pair? ele) (qq (option (@ (value (unquote (tail ele)))) (unquote (first ele))))
          (list (q option) ele)))
      arg))

  (define* (html-sxml-list->list arg #:optional ordered?) "list boolean -> sxml"
    (pair (if ordered? (q ol) (q ul))
      (map (l (ele) (list (q li) (if (list? ele) (html-sxml-list->list ele) (any->string ele))))
        arg)))

  (define (html-sxml-list->table arg) "(list/pair:columns ...) -> sxml"
    (pair (q table)
      (map
        (l (ele)
          (pair (q tr) (map (l (ele) (list (q td) ele)) (if (pair? ele) (pair->list ele) ele))))
        arg))))