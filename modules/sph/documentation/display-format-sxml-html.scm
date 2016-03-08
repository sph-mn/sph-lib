(library (sph documentation display-format-sxml-html)
  (export
    display-format-sxml-html)
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
  ;todo: module name for each binding

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

  (define (library-documentation-sxml-html module indent-level) "((symbol ...) ...) -> list"
    ;this feature does not really belong into sph-cms though
    (let (module (list-sort-with-accessor string<? first module))
      (list
        (if (> (length module) 2)
          (pair (q ul)
            (map (compose (l (e) (list (q li) e)) create-binding-name-anchor first) module))
          (list))
        (pair (q div)
          (map (l (binding) (library-documentation-sxml-html-section indent-level binding)) module)))))

  (define display-format-sxml-html
    (alist-quoted-merge-key/value display-format-plist format-module-documentation
      (l (name md) md) format-modules-documentation
      (l (mds) (library-documentation-sxml-html (apply append mds) 0))))

  (set! documentation-display-formats
    (pair (pair (q sxml-html) display-format-sxml-html) documentation-display-formats)))
