(library (sph documentation display-format-sxml-html)
  (export
    library-documentation-sxml-html)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph documentation)
    (sph documentation display-format-plist)
    (sph lang docl itml-to-sxml-html)
    (sph list)
    (sph one)
    (sph string)
    (sph web sxml-html)
    (except (srfi srfi-1) map))

  ;wip
  ;different display for multiple-modules and single-module

  (define (create-binding-name-anchor-target title)
    (qq (span (@ (id "b-" (unquote title))) (unquote title))))

  (define (create-binding-name-anchor title)
    (qq (a (@ (href "#b-" (unquote title))) (unquote title))))

  (define library-documentation-sxml-html-section
    (l (nesting-level binding)
      (section nesting-level (create-binding-name-anchor-target (first binding))
        (filter-map
          (l (e)
            (let (content (remove string-null? (tail e)))
              (if (null? content) #f
                (let (lines (append-map (l (e) (string-split e #\newline)) content))
                  (section nesting-level (first e) (process-lines lines))))))
          (tail binding))
        (q (class "doc-bi")))))

  (define (library-binding-info-plist module-name)
    (alist-quoted-bind display-format-plist (format-binding-info format-arguments)
      (map
        (l (binding-info)
          (append
            (format-binding-info binding-info
              (format-arguments (bi-arguments binding-info) (bi-type binding-info)))
            (list (pair (q module) module-name))))
        (module-binding-info module-name))))

  (define (documentation-library- module-names nesting-level)
    "((symbol ...) ...) integer -> list"
    (fold-multiple
      (l (module-name)
        (let (module-bi (library-binding-info-plist module-name)) (map first module-bi)
          (list
            (if (> (length module) 2)
              (pair (q ul)
                (map (compose (l (e) (list (q li) e)) create-binding-name-anchor first) module))
              (list))
            (pair (q div)
              (map (l (binding) (library-documentation-sxml-html-section nesting-level binding))
                module)))))
      module-names))

  (define (documentation-library-index library-names)
    #t

    )

  )
