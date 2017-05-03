(library (sph module)
  (export
    call-if-defined
    collect-library-names
    current-bindings
    current-module-ref
    environment*
    export-modules
    file-contains-module?
    find-modules
    find-modules-by-name
    import!
    import-any
    import-directory-tree
    import-if-existent
    import-unexported
    include-file-from-load-path
    library-exists?
    load-with-environment
    module-compose
    module-dependencies
    module-dependencies-pairs
    module-exports
    module-name->load-path+full-path&
    module-name->path
    module-name-interface-fold
    module-name-interface-map
    module-names->exports
    module-ref-no-error
    path->load-path
    path->module-name
    path->symbol-list
    path-drop-load-path)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (rnrs eval)
    (sph)
    (sph error)
    (sph filesystem)
    (only (ice-9 regex) string-match)
    (only (rnrs sorting) list-sort)
    (only (sph conditional) if-pass)
    (only (sph read-write) file->datums)
    (only (sph string) string-longest-prefix string-drop-prefix)
    (only (srfi srfi-1)
      append-map
      last
      find))

  (define module-interface->module (compose resolve-module module-name))

  (define (module-dependencies module) "module -> (module ...)"
    (map module-interface->module (module-uses module)))

  (define (module-dependencies-pairs module) "module -> ((module . module:dependency) ...)"
    (map (l (e) (pair module e)) (module-dependencies module)))

  (define (module-exports module)
    "module -> (symbol ...)
    list of exported binding names"
    (module-map (l (name variable) name) (module-public-interface module)))

  (define (environment* . name)
    "(symbol ...) ... -> environment/module
    this can be used to load modules that use syntax to create their module definition.
    the modules contents are first evaluated in the top-level environment - before the environment object is created.
    the syntax used for creating the module definition must be available in the current top-level environment.
    only the \".scm\" filename-extension is supported when resolving file paths from module names for loading"
    (map
      (l (e)
        (load
          (module-name->load-path+full-path& e ".scm"
            %load-path (l (load-path full-path) full-path))))
      name)
    (apply environment name))

  (define (module-name-interface-fold proc init module-name)
    "procedure:{name any:value any:init} any (symbol ...) -> any
    fold over the exported, bound variables for the given module-name"
    (hash-fold (l (key val r) (if (variable-bound? val) (proc key (variable-ref val) r) r)) init
      (module-obarray (resolve-interface module-name))))

  (define (module-name-interface-map proc module-name)
    "procedure:{key value -> any} (symbol ...) -> list"
    (module-name-interface-fold (l (key value r) (pair (proc key value) r)) (list) module-name))

  (define (module-names->exports a) "((symbol ...) ...) -> (list:module-bindings ...)"
    (map (compose module-exports resolve-module) a))

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

  (define-syntax-rule (export-module into-module name)
    (module-re-export! into-module
      (filter (lambda (e) (not (eqv? e (quote %module-public-interface))))
        (module-map (lambda args (car args)) (resolve-interface name)))))

  (define (default-before-filter name) (string-suffix? ".scm" name))

  (define (collect-library-names path)
    "string -> list
    extracts library names from \"*.scm\" files under path"
    (fold-directory-tree
      (l (e stat-info r)
        (if (and (string-suffix? ".scm" e) (eqv? (q regular) (stat:type stat-info)))
          (match (call-with-input-file e read) (((quote library) name _ ...) (pair name r)) (_ r)) r))
      (list) path (inf)))

  (define*
    (find-modules base-path #:key (max-depth (inf))
      (load-path (string-longest-prefix base-path %load-path)))
    "get module-names corresponding to existing files that may contain libraries in directory \"base-path\" and subdirectories.
    path must be in the load-path.
    files must have a \".scm\" suffix.
    file contents are not checked, the result may include files that contain no library definition"
    (if load-path
      (let (base-path-stat (false-if-exception (stat base-path)))
        (if base-path-stat
          (if (eqv? (q regular) (stat:type base-path-stat)) (list (path->module-name base-path))
            (fold-directory-tree
              (l (e stat-info r)
                (if (and (string-suffix? ".scm" e) (eqv? (q regular) (stat:type stat-info)))
                  (pair (path->module-name e) r) r))
              (list) base-path max-depth))
          (list)))
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
    ;imports and defines the binding locally at place.
    ;unquoted-module-name unquoted-binding-name ->
    (define binding-name (@@ module-name binding-name)))

  (define*
    (import-directory-tree path #:key (resolve-interface-args (list)) #:rest find-modules-args)
    "string #:key (max-depth integer) (resolve-interface-args list/procedure) -> unspecified
    imports libraries with find-modules"
    (let (current-module* (current-module))
      (each
        (l (e)
          (module-use! current-module*
            (apply resolve-interface e
              (if (procedure? resolve-interface-args) (resolve-interface-args e)
                resolve-interface-args))))
        (apply find-modules path find-modules-args))))

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

  (define* (module-name->path a #:optional (filename-extension ".scm"))
    "(symbol ...) string -> string
    creates a filesystem path from a module name. module existence is not checked.
    filename-extension can be false so that for example directory paths can be created"
    ( (l (a) (if filename-extension (string-append a filename-extension) a))
      (string-join (map symbol->string a) "/")))

  (define (file-contains-module? path)
    "string -> boolean
    true if file contains as the first expression an r6rs or r7rs library definition"
    (if (path->module-name path) #t #f))

  (define (path->module-name a)
    "string -> (symbol ...)/false
     checks the first expression for an r6rs or r7rs library definition"
    (match (call-with-input-file a read) ((library ((? symbol? name) ..1) _ ...) name)
      ((define-library ((? symbol? name) ..1) _ ...) name) (_ #f)))

  (define (not-scm-suffix? str) (not (string-suffix? ".scm" str)))

  (define (module-name->load-path+full-path& a filename-extension load-paths c)
    "(symbol ...) string (string ...) procedure:{string:load-path string:full-path -> any} -> any
    finds the load path under which a possibly partial (prefix) module name is saved.
    if no filename-extension is given it will usually only match directories"
    (let*
      ( (path (module-name->path a filename-extension))
        (r
          (any
            (l (load-path)
              (let (full-path (string-append load-path "/" path))
                (if (file-exists? full-path) (list load-path full-path) #f)))
            load-paths)))
      (and r (apply c r))))

  (define* (path-drop-load-path a #:optional (load-paths %load-path))
    "string [list] -> string
    remove from the beginning of a path the first load-path path exists in.
    returns the input path as is if no load-path contains it"
    (or
      (any (l (b) (and (string-prefix? b a) (string-drop-prefix (ensure-trailing-slash b) a)))
        load-paths)
      a))

  (define (path->load-path a) "returns one load-path where a load-path-relative path can be found"
    (any (l (load-path) (and (file-exists? (string-append load-path "/" a)) load-path)) %load-path))

  (define (module-ref-no-error module name)
    "like guiles module-ref but results in false and does not raise an error if the variable is unbound"
    (if-pass (module-variable module name) (l (a) (variable-ref a))))

  (define (call-if-defined module name)
    (if-pass (module-variable module name) (l (a) ((variable-ref a)))))

  (define (path->symbol-list a)
    "create a module name from a typical path string. for example \"/a/b/c\" -> (a b c)"
    (let (a (string-trim-both a #\/))
      (if (string-null? a) (list) (map string->symbol (string-split a #\/)))))

  (define (find-modules-by-name name search-type load-paths)
    "(symbol ...) symbol:exact/prefix/prefix-not-exact (string ...) -> ((symbol ...):module-name ...)
    find module names by module name part"
    (let
      ( (search
          (l (name filename-extension) "list string/boolean -> list"
            (or
              (module-name->load-path+full-path& name filename-extension
                load-paths
                (l (load-path full-path) "(string ...) string -> list"
                  (find-modules full-path #:load-path load-path)))
              (list))))
        (filename-extensions
          (append
            (if (or (eqv? (q prefix) search-type) (eqv? (q prefix-not-exact) search-type))
              (list #f) (list))
            (if (or (eqv? (q prefix) search-type) (eqv? (q exact) search-type)) (list ".scm")
              (list)))))
      (append-map (l (e) (search name e load-paths)) filename-extensions))))
