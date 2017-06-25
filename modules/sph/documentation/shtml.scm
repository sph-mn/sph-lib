(library (sph documentation shtml)
  (export
    documentation-shtml-libraries
    documentation-shtml-library)
  (import
    (guile)
    (ice-9 threads)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph documentation)
    (sph documentation display-format-plist)
    (sph list)
    (sph module)
    (sph one)
    (sph string)
    (sph web shtml)
    (only (srfi srfi-1) alist-delete)
    (only (sph lang docl itml-to-shtml) process-lines))

  (define (create-binding-name-anchor-target title)
    (qq (span (@ (id "b-" (unquote title))) (unquote title))))

  (define (create-binding-name-anchor title)
    (qq (a (@ (href "#b-" (unquote title))) (unquote title))))

  (define (library-documentation-shtml-binding-documentation binding nesting-depth)
    (shtml-section nesting-depth (create-binding-name-anchor-target (first binding))
      (filter-map
        (l (e)
          (let (content (remove string-null? (tail e)))
            (if (null? content) #f
              (let (lines (append-map (l (e) (string-split e #\newline)) content))
                (shtml-section (+ 1 nesting-depth) (first e)
                  (process-lines lines) (list (q class) (first e)))))))
        (tail binding))))

  (define (get-binding-documentation module-name) "list -> ((symbol:name . list:alist) ...)"
    (list-sort-with-accessor string<? first
      (alist-bind display-format-plist (format-binding-info format-arguments)
        (map
          (l (binding-info)
            (format-binding-info binding-info
              (format-arguments (bi-arguments binding-info) (bi-type binding-info))))
          (module-binding-info module-name)))))

  (define (documentation-shtml-library nesting-depth . library-names)
    "((symbol ...) ...) integer -> list
     a navigatable index of all bindings in a module and a listing of the available binding documentation"
    (let (bindings (append-map get-binding-documentation library-names))
      (letpar
        ( (index
            (pairs (q ul) (q (@ (class "doc-n")))
              (map (compose (l (e) (list (q li) e)) create-binding-name-anchor first) bindings)))
          (content
            (pairs (q div) (q (@ (class "doc-b")))
              (map
                (l (e)
                  (library-documentation-shtml-binding-documentation
                    (pair (first e) (alist-delete "module" (tail e))) nesting-depth))
                bindings))))
        (list index content))))

  (define*
    (documentation-shtml-libraries library-names #:optional
      (map-binding-name (l (name library-name) (symbol->string name)))
      (map-library-name any->string))
    "((symbol ...) ...) [{symbol list:library-name -> sxml} {list:library-name -> sxml}] -> list
     a table of all bindings from all specified libraries with binding names in the first column and associated library names in the second"
    (let
      (binding-info
        (map tail
          (list-sort-with-accessor string<? first
            (append-map
              (l (names library-name)
                (map
                  (l (name)
                    (list (symbol->string name) (map-binding-name name library-name)
                      (map-library-name library-name)))
                  names))
              (map (l (a) (module-exports (resolve-interface a))) library-names) library-names))))
      (pair (q table) (map (l (e) (pair (q tr) (map (l (e) (list (q td) e)) e))) binding-info)))))
