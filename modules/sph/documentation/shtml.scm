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
    (sph documentation)
    (sph documentation display-format-plist)
    (sph list)
    (sph hashtable)
    (sph module)
    (sph module binding-info)
    (sph other)
    (sph string)
    (sph web shtml)
    (only (srfi srfi-1) alist-delete))

  (define shtml-lines
    (let*
      ( (inline-html-tags
          (ht-from-list
            (map (l (a) (pair a a))(list-q span a object img script select button input label select textarea))))
        (inline-html-tag? (l (a) (ht-ref inline-html-tags a))) (line-wrap (l (a) (list (q p) a)))
        (line-list (l (a) (pair (q p) a)))
        (splice-non-tag-lists (l (a) (splice (l (a) (or (null? a) (not (symbol? (first a))))) a))))
      (l (a)
        "list integer -> list
        receives a list of expressions that eventually become separate lines.
        rules:
        * html inline elements are wrapped with <p>
        * html block elements are left as is
        * list contents become <p> contents
        * other elements are wrapped with <p>
        * on the first level of the given list, lists that
          do not correspond to html tags are spliced.
          these lists are assumed to contain result elements
          from dynamic code evaluation"
        (let loop ((a (splice-non-tag-lists a)))
          (if (null? a) a
            (let ((a (first a)) (b (loop (tail a))))
              (if (list? a)
                (let (a (splice-non-tag-lists a))
                  (if (null? a) b
                    (let (prefix (first a))
                      (pair
                        (if (symbol? prefix) (if (inline-html-tag? prefix) (line-wrap a) a)
                          (line-list a))
                        b))))
                (pair (line-wrap a) b))))))))

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
              (case key
                ((type) (symbol->string (first content)))
                (else
                  (let (content (string-join content "\n"))
                    (shtml-section (+ 1 nesting-depth) key
                      (shtml-lines (string-split content #\newline)) (list (q class) key))))))))
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

  (define* (doc-shtml-libraries libraries #:optional (map-data identity))
    "create a table of all bindings in all specified libraries.
     first column: binding name
     second column: library name"
    (shtml-list->table
      (map-apply map-data
        (list-sort-with-accessor string<? (compose symbol->string first)
          (doc-bindings libraries list))))))
