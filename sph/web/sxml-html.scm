(library (sph web sxml-html)
  (export
    sxml-html-alist->options
    sxml-html-hyperlink
    sxml-html-include-script
    sxml-html-include-style
    sxml-html-indent
    sxml-html-indent-create
    sxml-html-list->list
    sxml-html-list->table
    sxml-html-text->sxml)
  (import
    (rnrs base)
    (sph)
    (only (guile) string-split make-list)
    (only (sph list) interleave pair->list)
    (only (sph string) any->string))

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

  (define (sxml-html-alist->options arg) "((content . value/false) ...) -> list"
    (map
      (l (ele)
        (if (pair? ele) (qq (option (@ (value (unquote (tail ele)))) (unquote (first ele))))
          (list (q option) ele)))
      arg))

  (define* (sxml-html-list->list arg #:optional ordered?) "list boolean -> sxml"
    (pair (if ordered? (q ol) (q ul))
      (map (l (ele) (list (q li) (if (list? ele) (sxml-html-list->list ele) (any->string ele))))
        arg)))

  (define (sxml-html-list->table arg) "(list/pair:columns ...) -> sxml"
    (pair (q table)
      (map
        (l (ele)
          (pair (q tr) (map (l (ele) (list (q td) ele)) (if (pair? ele) (pair->list ele) ele))))
        arg))))