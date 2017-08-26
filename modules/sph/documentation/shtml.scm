(library (sph documentation shtml)
  (export
    doc-shtml-libraries
    doc-shtml-library)
  (import
    (guile)
    (ice-9 threads)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph documentation)
    (sph documentation display-format-plist)
    (sph lang itml eval shtml)
    (sph list)
    (sph module)
    (sph one)
    (sph string)
    (sph web shtml)
    (only (srfi srfi-1) alist-delete))

  (define (create-binding-name-anchor-target title)
    (qq (span (@ (id "b-" (unquote title))) (unquote title))))

  (define (create-binding-name-anchor binding)
    (let ((title (first binding)) (type (or (alist-ref (tail binding) (q type)) "")))
      (qq (a (@ (href "#b-" (unquote title)) (class (unquote type))) (unquote title)))))

  (define (shtml-bindings binding nesting-depth)
    (shtml-section nesting-depth (create-binding-name-anchor-target (first binding))
      (filter-map
        (l (a)
          (let ((key (first a)) (content (tail a)))
            (if (null? content) #f
              (case key ((type) (symbol->string (first content)))
                (else
                  (let
                    (lines
                      (remove string-null? (append-map (l (a) (string-split a #\newline)) content)))
                    (if (null? lines) ""
                      (shtml-section (+ 1 nesting-depth) key
                        (if (eq? (q signature) key) (list (q pre) (string-join lines "\n"))
                          (itml-shtml-lines lines))
                        (list (q class) (first a))))))))))
        (list-sort-with-accessor string>? (compose symbol->string first) (tail binding)))))

  (define (get-bindings module-name) "list -> ((symbol:name . list:alist) ...)"
    (list-sort-with-accessor string<? first
      (alist-bind display-format-plist (format-binding-info format-arguments)
        (map
          (l (binding-info)
            (format-binding-info binding-info
              (format-arguments (bi-arguments binding-info) (bi-type binding-info))))
          (module-binding-info module-name)))))

  (define (string-indent-to-nbsp a)
    (let (index (string-skip a #\space))
      (if (< 0 index) (list (make-list index (q (*ENTITY* "nbsp"))) (string-drop a index)) a)))

  (define (doc-shtml-library nesting-depth library-name)
    "integer (symbol ...) -> list
     a navigatable index of all bindings from the specified libraries and
     a listing of the available documentation for bindings"
    (let ((bindings (get-bindings library-name)) (description (module-description library-name)))
      (letpar
        ( (index
            (pairs (q ul) (q (@ (class "doc-n")))
              (map (compose (l (a) (list (q li) a)) create-binding-name-anchor) bindings)))
          (description
            (and description
              (map (l (a) (list (q p) a))
                (map string-indent-to-nbsp (string-split description #\newline)))))
          (content
            (pairs (q div) (q (@ (class "doc-b")))
              (map
                (l (a)
                  (shtml-bindings (pair (first a) (alist-delete "module" (tail a))) nesting-depth))
                bindings))))
        (list description index content))))

  (define*
    (doc-shtml-libraries library-names #:optional
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
      (pair (q table) (map (l (a) (pair (q tr) (map (l (a) (list (q td) a)) a))) binding-info)))))
