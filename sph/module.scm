(library (sph module)
  (export
    %search-load-path-regexp
    call-if-defined
    current-bindings
    directory-tree-module-names
    export-modules
    import!
    import-any
    import-directory-tree
    import-if-existent
    import-unexported
    include-file-from-load-path
    library-exists?
    load-with-environment
    module-compose
    module-name->path
    module-ref-no-error
    path->module-name
    path->symbol-list)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph filesystem)
    (only (ice-9 regex) string-match)
    (only (rnrs sorting) list-sort)
    (only (sph conditional) pass-if)
    (only (sph read-write) file->datums)
    (only (sph string) string-longest-prefix)
    (only (srfi srfi-1) last))

  (define-syntax-rule (current-bindings)
    ;return a guile-hashtable of all bindings in the current module
    (module-obarray (current-module)))

  (define (module-re-export-all module)
    (module-re-export! module
      (filter (lambda (e) (not (eqv? e (quote %module-public-interface))))
        (apply append (map (l (e) (module-map (lambda args (car args)) e)) (module-uses module))))))

  (define-syntax-rule (module-compose target-name unquoted-import-spec ...)
    ;unquoted-import-spec is like for r6rs (import)
    (begin (define-module target-name) (import unquoted-import-spec ...)
      (module-re-export-all (current-module))))

  (define-syntax-rule (export-modules unquoted-name ...)
    ((lambda (m) (export-module m (q unquoted-name)) ...) (current-module)))

  (define-syntax-rule (export-module with-module name)
    (module-re-export! with-module
      (filter (lambda (ele) (not (eqv? ele (quote %module-public-interface))))
        (module-map (lambda args (car args)) (resolve-interface name)))))

  (define (default-before-filter name) (string-suffix? ".scm" name))

  (define*
    (directory-tree-module-names base-path #:optional (max-depth (inf))
      (before-filter default-before-filter))
    "list probable libraries belonging to directory \"path\" and subdirectories. path must be in the load-path.
    result may include files that contain no library definition, depending on the validations in before-filter.
    the default before-filter allows only files with a \".scm\" suffix."
    (let (load-path (string-longest-prefix base-path %load-path))
      (if load-path
        (fold-directory-tree
          (l (ele stat-info prev)
            (if (eq? (q regular) (stat:type stat-info))
              (pair (path->symbol-list (string-drop ele (string-length load-path))) prev) prev))
          (list) base-path #:max-depth max-depth #:before-filter before-filter)
        (raise (q path-is-not-in-load-path)))))

  (define (import-any . module-names) "import any existing libraries of list"
    (any
      (l (ele)
        (if (library-exists? ele) (begin (module-use! (current-module) (resolve-interface ele)) #t)
          #f))
      module-names))

  (define (import! . modules) "import modules at run time"
    (each (l (ele) (module-use! (current-module) (resolve-interface ele))) modules))

  (define*
    (import-directory-tree path #:optional (max-depth (inf)) (before-filter default-before-filter)
      (resolve-interface-args (list)))
    "string #:key (max-depth integer) (resolve-interface-args list/procedure) -> unspecified
    imports libraries with directory-tree-module-names."
    (let (current-module* (current-module))
      (each
        (l (ele)
          (module-use! current-module*
            (apply resolve-interface ele
              (if (procedure? resolve-interface-args) (resolve-interface-args ele)
                resolve-interface-args))))
        (directory-tree-module-names path max-depth before-filter))))

  (define (import-if-existent . module-names)
    (map (l (ele) (module-use! (current-module) (resolve-interface ele)))
      (filter library-exists? module-names)))

  (define-syntax-rule (import-unexported module-name binding-name)
    (define binding-name (@@ module-name binding-name)))

  (define-syntax include-file-from-load-path
    (lambda (s)
      "literal-string -> expressions
      include contents of a scheme source file, searching for filename in the current load-path.
      similar to guiles include-from-path"
      (syntax-case s ()
        ( (_ filename)
          (let ((path (%search-load-path (syntax->datum (syntax filename)))))
            (quasisyntax (unsyntax (datum->syntax s (pair (quote begin) (file->datums path))))))))))

  (define (library-exists? name) (%search-load-path (module-name->path name)))

  (define (load-with-environment path env)
    "load filename and evaluate its contents with the given eval environment which may be a module, a r6rs library or a standard environment."
    (let (port (open-file path "r"))
      (let loop ((expr (read port)) (prev #f))
        (if (eof-object? expr) prev (loop (read port) (eval expr env))))
      (close port)))

  (define (%search-load-path-regexp arg)
    (any
      (l (load-path)
        ( (l (res) (if (null? res) #f (first (list-sort string< res))))
          (directory-tree-paths load-path (l (ele) (not (string-match arg ele))))))
      %load-path))

  (define* (module-name->path arg #:optional (relative #f))
    "create a filesystem path string from a module name.t
    if \"relative\" is false, load-path is searched and a full path is returned on success."
    ( (if relative identity %search-load-path)
      (string-append (string-join (map symbol->string arg) "/") ".scm")))

  (define (not-scm-suffix? str) (not (string-suffix? ".scm" str)))

  (define (module-ref-no-error module name)
    "like guiles module-ref but results in false and does not raise an error if variable is unbound"
    (pass-if (module-variable module name) (l (a) (variable-ref a))))

  (define (call-if-defined module name)
    (pass-if (module-variable module name) (l (a) ((variable-ref a)))))

  (define* (path->module-name arg #:optional (load-paths %load-path))
    "string -> (symbol ...)\false
    create a module name from a typical path string by searching load-path for path if path is a full path,
    removing the load-path portion and converting the result to a symbol-list.
    does not check if the file actually is a regular file or a library. use %search-load-path in combination
    with path->module-name"
    (if (string-prefix? "/" arg)
      (let*
        ( (arg (string-trim-right arg #\/))
          (load-path
            (string-longest-prefix arg (map (l (ele) (string-trim-right ele #\/)) load-paths))))
        (if load-path (path->symbol-list (string-drop arg (string-length load-path))) load-path))
      (path->symbol-list arg)))

  (define (path->symbol-list arg)
    "create a module name from a typical path string. for example \"/a/b/c\" -> (a b c)"
    (let ((arg (string-trim-both (remove-filename-extension arg) #\/)))
      (if (string-null? arg) (list) (map string->symbol (string-split arg #\/))))))