(library (sph documentation itpn)
  (export
    documentation-itpn-libraries
    documentation-itpn-library)
  (import
    (guile)
    (ice-9 threads)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph module binding-info)
    (sph documentation)
    (sph documentation display-format-plist)
    (sph list)
    (sph module)
    (sph other)
    (sph string)
    (except (srfi srfi-1) map))

  (define (create-newline-indent size) (string-append "\n" (create-indent size)))

  (define (with-binding-info-title-and-lines-proc c)
    (l (binding-info)
      (let (content (remove string-null? (tail binding-info)))
        (if (null? content) #f
          (c (first binding-info) (append-map (l (e) (string-split e #\newline)) content))))))

  (define (get-binding-documentation module-name) "list -> ((symbol:name . list:alist) ...)"
    (list-sort-with-accessor string<? first
      (alist-bind display-format-plist (format-binding-info format-arguments)
        (map
          (l (binding-info)
            (format-binding-info binding-info
              (format-arguments (bi-arguments binding-info) (bi-type binding-info))))
          (module-binding-info module-name)))))

  (define
    (library-documentation-itpn-binding-documentation name binding-info newline-indent-3
      newline-indent-4)
    (string-join
      (pair name
        (filter-map
          (with-binding-info-title-and-lines-proc
            (l (title lines) (string-join (pair title lines) newline-indent-4)))
          binding-info))
      newline-indent-3))

  (define (documentation-itpn-library nesting-depth . library-names)
    "((symbol ...) ...) integer -> string
     index of all bindings in a module and a listing of the available binding documentation"
    (let
      ( (bindings (append-map get-binding-documentation library-names))
        (newline-indent-1 (create-newline-indent nesting-depth))
        (newline-indent-2 (create-newline-indent (+ 1 nesting-depth)))
        (newline-indent-3 (create-newline-indent (+ 2 nesting-depth)))
        (newline-indent-4 (create-newline-indent (+ 3 nesting-depth))))
      (letpar
        ( (index (map first bindings))
          (content
            (map
              (l (e)
                (library-documentation-itpn-binding-documentation (first e)
                  (alist-delete "module" (tail e)) newline-indent-3 newline-indent-4))
              bindings)))
        (string-append "index" (string-join index newline-indent-2 (q prefix))
          newline-indent-1 "details" (string-join content newline-indent-2 (q prefix))))))

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
              (map (compose module-exports resolve-interface) library-names) library-names))))
      (string-join (map (l (e) (string-join e " ")) binding-info)
        (string-append "\n" (string-multiply "  " nesting-depth))))))
