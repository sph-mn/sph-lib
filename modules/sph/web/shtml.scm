(define-module (sph web shtml))
(use-modules (rnrs io ports) (sph) (sph list) (sxml simple) ((sph string) #:select (any->string)))

(export shtml->html shtml-alist->options
  shtml-heading shtml-hyperlink
  shtml-include-css shtml-include-javascript
  shtml-indent shtml-indent-create shtml-list->list shtml-list->table shtml-section shtml-text->sxml)

(define sph-web-shtml-description "helpers to create html as sxml")
(define html-headings #(h1 h2 h3 h4 h5 h6))

(define (shtml-heading depth . content)
  "integer sxml -> sxml
   create a html heading element, for example <h1>, with the given content"
  (pair (vector-ref html-headings (min 5 depth)) content))

(define (shtml-section depth title content . attributes)
  "integer sxml sxml (string/symbol string/symbol) ... -> sxml
   create the sxml for an html <section> tag with attributes, heading and content in a single html tag.
   content is put in a <div> unless it already is contained in single tag or if it is empty.
   the single tag is ensured to make accessors for the content area (everything not first heading) simpler"
  (pair (q section)
    (append (if (null? attributes) attributes (list (pair (q @) attributes)))
      (pair (shtml-heading depth title)
        (if (list? content)
          (if (null? content) (list)
            (if (symbol? (first content)) (list content) (list (pair (q div) content))))
          (if (and (string? content) (string-null? content)) (list) (list (q div) content)))))))

(define shtml-indent (q ((*ENTITY* "#160") (*ENTITY* "#160"))))

(define (shtml-indent-create depth)
  "integer -> sxml
   creates indent with the html entity for the space character so it does not get compressed by the viewer"
  (apply append (make-list depth shtml-indent)))

(define (shtml-text->sxml a)
  "string -> sxml
   replace newlines with (br)"
  (interleave (string-split a #\newline) (q (br))))

(define* (shtml-include-javascript path #:optional is-async)
  "string boolean -> sxml
   create the shtml for including a javascript file"
  (qq
    (script
      (@ (src (unquote path)) (unquote-splicing (if is-async (list (q (async async))) (list)))) "")))

(define (shtml-include-css path)
  "string -> sxml
   create the shtml for including a stylesheet file"
  (qq (link (@ (rel "stylesheet") (type "text/css") (href (unquote path))))))

(define* (shtml-hyperlink target title #:optional (attributes (list)))
  "string string -> sxml
   sxml for an html <a>"
  (qq (a (@ (href (unquote target)) (unquote-splicing attributes)) (unquote (or title target)))))

(define (shtml-alist->options a)
  "((content . string:value/false)/string ...) -> sxml:((option _ ...) ...)
   create the shtml for multiple <option> elements"
  (map
    (l (a)
      (if (pair? a) (qq (option (@ (value (unquote (tail a)))) (unquote (first a))))
        (list (q option) a)))
    a))

(define* (shtml-list->list a #:optional ordered?)
  "(sxml/list:sub-list) boolean -> sxml
   create the shtml for an unordered or ordered list structure, <ul> or <ol>, with elements.
   input list elements that are lists are recursively created as shtml sublists"
  (pair (if ordered? (q ol) (q ul))
    (map (l (a) (list (q li) (if (list? a) (shtml-list->list a) a))) a)))

(define (shtml-list->table a)
  "((sxml:cell ...) ...) -> sxml
   create the shtml for a <table> with content"
  (pair (q table) (map (l (a) (pair (q tr) (map (l (a) (list (q td) a)) a))) a)))

(define (shtml->html shtml port)
  "write html from shtml to port, adding a <!doctype html> declaration at the beginning"
  (put-string port "<!doctype html>") (sxml->xml shtml port))
