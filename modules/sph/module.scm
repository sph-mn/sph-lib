(library (sph module)
  (export
    call-if-defined
    current-bindings
    current-module-ref
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
    module-interface-binding-names
    module-name->path
    module-name-interface-fold
    module-name-interface-map
    module-names->interface-binding-names
    module-ref-no-error
    path->module-name
    path->module-names
    path->symbol-list)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph error)
    (sph filesystem)
    (only (ice-9 regex) string-match)
    (only (rnrs sorting) list-sort)
    (only (sph conditional) pass-if)
    (only (sph read-write) file->datums)
    (only (sph string) string-longest-prefix)
    (only (srfi srfi-1) last))

  (define (module-name-interface-fold proc init module-name)
    "procedure:{name any:value any:init} any (symbol ...) -> any
    fold over the exported, bound variables for the given module-name"
    (hash-fold (l (key val r) (if (variable-bound? val) (proc key (variable-ref val) r) r)) init
      (module-obarray (resolve-interface module-name))))

  (define (module-name-interface-map proc module-name)
    "procedure:{key value -> any} (symbol ...) -> list"
    (module-name-interface-fold (l (key value r) (pair (proc key value) r)) (list) module-name))

  (define (module-interface-binding-names module)
    (module-map (l (name variable) name) (module-public-interface module)))

  (define (module-names->interface-binding-names a) "list -> _"
    (map (compose module-interface-binding-names resolve-module) a))

  (define-syntax-rule (current-bindings)
    ;return a guile-hashtable of all bindings in the current module
    (module-obarray (current-module)))

  (define (module-re-export-all module)
    (module-re-export! module
      (filter (l (e) (not (eqv? e (quote %module-public-interface))))
        (apply append (map (l (e) (module-map (l a (first a)) e)) (module-uses module))))))

  (define-syntax-rule (module-compose target-name unquoted-import-spec ...)
    ;unquoted-import-spec is like for r6rs (import)
    (begin (define-module target-name) (import unquoted-import-spec ...)
      (module-re-export-all (current-module))))

  (define-syntax-rule (export-modules unquoted-name ...)
    ((lambda (m) (export-module m (q unquoted-name)) ...) (current-module)))

  (define-syntax-rule (export-module with-module name)
    (module-re-export! with-module
      (filter (lambda (e) (not (eqv? e (quote %module-public-interface))))
        (module-map (lambda args (car args)) (resolve-interface name)))))

  (define (default-before-filter name) (string-suffix? ".scm" name))

  (define*
    (path->module-names base-path #:key (max-depth (inf)) (before-filter default-before-filter)
      (load-path (string-longest-prefix base-path %load-path)))
    "list probable libraries belonging to directory \"path\" and subdirectories. path must be in the load-path.
    result may include files that contain no library definition, depending on the validations in before-filter.
    the default before-filter allows only files with a \".scm\" suffix."
    (if load-path
      (let
        (path->module-name (l (a)  (path->symbol-list (remove-filename-extension (string-drop a (string-length load-path)) (list "scm")))))
        (if (eqv? (q reqular) (stat:type (stat base-path))) (path->module-name base-path)
          (fold-directory-tree
            (l (e stat-info r)
              (if (eqv? (q regular) (stat:type stat-info)) (pair (path->module-name e) r) r))
            (list) base-path #:max-depth max-depth #:before-filter before-filter)))
      (error-create (q path-is-not-in-load-path))))

  (define (import-any . module-names)
    "(symbol ...) ... ->
    import any existing libraries of list"
    (any
      (l (e)
        (if (library-exists? e) (begin (module-use! (current-module) (resolve-interface e)) #t) #f))
      module-names))

  (define (import! . modules) "import modules at run time"
    (each (l (e) (module-use! (current-module) (resolve-interface e))) modules))

  (define-syntax-rule (import-unexported module-name binding-name)
    (define binding-name (@@ module-name binding-name)))

  (define*
    (import-directory-tree path #:key (resolve-interface-args (list)) #:rest
      path->module-names-args)
    "string #:key (max-depth integer) (resolve-interface-args list/procedure) -> unspecified
    imports libraries with path->module-names"
    (let (current-module* (current-module))
      (each
        (l (e)
          (module-use! current-module*
            (apply resolve-interface e
              (if (procedure? resolve-interface-args) (resolve-interface-args e)
                resolve-interface-args))))
        (apply path->module-names path path->module-names-args))))

  (define (current-module-ref a) "symbol -> any" (module-ref (current-module) a))

  (define (import-if-existent . module-names)
    (map (l (e) (module-use! (current-module) (resolve-interface e)))
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

  (define (library-exists? name) "(symbol ...) -> boolean"
    (%search-load-path (module-name->path name)))

  (define (load-with-environment path env)
    "load filename and evaluate its contents with the given eval environment which may be a module, a r6rs library or a standard environment."
    (let (port (open-file path "r"))
      (let loop ((expr (read port)) (r #f))
        (if (eof-object? expr) r (loop (read port) (eval expr env))))
      (close port)))

  (define* (module-name->path a #:optional (relative #f))
    "create a filesystem path string from a module name.t
    if \"relative\" is false, load-path is searched and a full path is returned on success."
    ( (if relative identity %search-load-path)
      (string-append (string-join (map symbol->string a) "/") ".scm")))

  (define (not-scm-suffix? str) (not (string-suffix? ".scm" str)))

  (define (module-ref-no-error module name)
    "like guiles module-ref but results in false and does not raise an error if variable is unbound"
    (pass-if (module-variable module name) (l (a) (variable-ref a))))

  (define (call-if-defined module name)
    (pass-if (module-variable module name) (l (a) ((variable-ref a)))))

  (define* path->module-name
    (let (path->symbol-list (l (a) (path->symbol-list (remove-filename-extension a (list "scm")))))
      (l* (a #:optional (load-paths %load-path))
        "string -> (symbol ...)\false
        create a module name from a typical path string by searching load-path for path if path is a full path,
        removing the load-path portion and converting the result to a symbol-list.
        does not check if the file actually is a regular file or a library. use %search-load-path in combination
        with path->module-name"
        (if (string-prefix? "/" a)
          (let*
            ( (a (string-trim-right a #\/))
              (load-path
                (string-longest-prefix a (map (l (e) (string-trim-right e #\/)) load-paths))))
            (if load-path (path->symbol-list (string-drop a (string-length load-path))) load-path))
          (path->symbol-list a)))))

  (define (path->symbol-list a)
    "create a module name from a typical path string. for example \"/a/b/c\" -> (a b c)"
    (let (a (string-trim-both a #\/))
      (if (string-null? a) (list) (map string->symbol (string-split a #\/))))))
