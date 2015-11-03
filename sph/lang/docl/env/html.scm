(library (sph lang docl env html)
  (export
    docl-env-heading
    docl-env-block-tree-row
    docl-env-block-tree-list
    docl-env-block-tree-olist
    hlink)
  (import
    (rnrs base)
    (guile)
    (sph)
    (only (sph list) any->list)
    (sph web sxml-html)
    (sph lang docl env default))

  (define html-headings (q #("" h1 h2 h3 h4 h5 h6)))

  (define (docl-env-heading depth . text)
    (list
      (cons (vector-ref html-headings (min 6 depth))
        text)))

  (define (docl-env-block-tree-row arg)
    (list
      (cons* (q div)
        (qq (@
            (class
              (unquote "row"))))
        (map (l (ele) (list (q div) ele)) (any->list arg)))))

  (define* (hlink name #:optional (target name)) "string string -> sxml
    create a html hyperlink <a> tag for name and target."
    (list (sxml-html-link name target))))