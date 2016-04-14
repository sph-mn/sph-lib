(library (sph documentation itpn)
  (export
    documentation-itpn-libraries
    documentation-itpn-library)
  (import
    (guile)
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph documentation)
    (sph documentation display-format-plist)
    (sph lang docl itml-to-itpn)
    (sph list)
    (sph module)
    (sph one)
    (sph string)
    (except (srfi srfi-1) map))

  (define (create-binding-name-anchor-target title)
    (qq (span (@ (id "b-" (unquote title))) (unquote title))))

  (define (create-binding-name-anchor title)
    (qq (a (@ (href "#b-" (unquote title))) (unquote title))))

  (define (library-documentation-itpn-binding-documentation binding indent)
    (string-append
      indent
      (symbol->string (first binding))

      )
    (itpn-section nesting-depth (create-binding-name-anchor-target )
      (filter-map
        (l (e)
          (let (content (remove string-null? (tail e)))
            (if (null? content) #f
              (let (lines (append-map (l (e) (string-split e #\newline)) content))
                (itpn-section (+ 1 nesting-depth) (first e)
                  (process-lines lines) (list (q class) (first e)))))))
        (tail binding))))

  (define (get-binding-documentation module-name) "list -> ((symbol:name . list:alist) ...)"
    (list-sort-with-accessor string<? first
      (alist-quoted-bind display-format-plist (format-binding-info format-arguments)
        (map
          (l (binding-info)
            (format-binding-info binding-info
              (format-arguments (bi-arguments binding-info) (bi-type binding-info))))
          (module-binding-info module-name)))))

  (define (documentation-itpn-library nesting-depth . library-names)
    "((symbol ...) ...) integer -> string
    index of all bindings in a module and a listing of the available binding documentation"
    (let
      ( (bindings (append-map get-binding-documentation library-names))
        (indent (string-multiply "  " nesting-depth))
        (indent-next (string-multiply "  " (+ 1 nesting-depth))))
      (par-let
        ( (index
            (string-join (map (compose symbol->string first) bindings)
              (string-append "\n" indent-next)))
          (content
            (map
              (l (e)
                (library-documentation-itpn-binding-documentation
                  (pair (first e) (alist-delete "module" (tail e))) (+ 1 nesting-depth)))
              bindings)))
        (list index content))))

  (define*
    (documentation-itpn-libraries library-names #:optional (nesting-depth 0)
      (map-binding-name (l (name library-name) (symbol->string name)))
      (map-library-name any->string))
    "((symbol ...) ...) integer [{symbol list:library-name -> string} {list:library-name -> string}] -> string
    list of binding and library name"
    (let
      (binding-info
        ;((mapped-binding-name mapped-library-name) ...)
        (map tail
          (list-sort-with-accessor string<? first
            (append-map
              (l (names library-name)
                (map
                  (l (name)
                    (list (symbol->string name) (map-binding-name name library-name)
                      (map-library-name library-name)))
                  names))
              (module-names->exports library-names) library-names))))
      (string-join (map (l (e) (string-join e " ")) binding-info)
        (string-append "\n" (string-multiply "  " nesting-depth))))))
