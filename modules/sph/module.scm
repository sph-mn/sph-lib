(library (sph module)
  (export
    environment*
    import-unexported
    load-with-environment
    module-compose
    module-current-bindings
    module-dependencies
    module-exports
    module-file->name
    module-file?
    module-find
    module-find-by-name
    module-find-one
    module-fold
    module-match-guile-definition
    module-match-rnrs-definition
    module-name->load-path-and-path
    module-re-export-modules
    path->load-path
    sph-module-description
    symbol-list->path)
  (import
    (guile)
    (ice-9 ftw)
    (ice-9 match)
    (rnrs eval)
    (sph)
    (sph string)
    (only (sph filesystem) realpath* remove-trailing-slash)
    (only (srfi srfi-1) append-map))

  (define sph-module-description
    "guile module system and rnrs library related procedures
     # syntax
     module-compose :: dest source ...
     define a new module dest with all exported bindings of the specified sources.
     example: (module-compose (my-module) (rnrs base) (rnrs sorting))")

  (define* (file->datums path #:optional (get-datum read))
    "string procedure:reader -> list
     read all scheme datums of a file specified by path"
    ; copied from (sph io) to avoid circular dependecy
    (call-with-input-file path
      (l (port)
        (let loop ((a (get-datum port)))
          (if (eof-object? a) (list) (pair a (loop (get-datum port))))))))

  ;-- querying

  (define-syntax-rule (module-current-bindings)
    ; return a guile-hashtable of all bindings in the current module
    (module-obarray (current-module)))

  (define (module-dependencies module) "module -> (module ...)"
    (map (compose resolve-module module-name) (module-uses module)))

  (define (module-exports module)
    "module -> (symbol ...)
     list of exported binding names"
    (module-map (l (name variable) name) module))

  (define (module-fold proc init module)
    "procedure:{name any:value any:init} any (symbol ...) -> any
     fold over the exported, bound variables for the given module-name"
    (hash-fold (l (key val r) (if (variable-bound? val) (proc key (variable-ref val) r) r)) init
      (module-obarray module)))

  ;-- creation and modification

  (define-syntax-rule (module-compose dest-name r6rs-import-spec ...)
    (begin (define-module dest-name) (module-re-export-modules r6rs-import-spec ...)))

  (define-syntax-rule (module-re-export-modules r6rs-import-spec ...)
    (begin (import r6rs-import-spec) ...
      (let (m (current-module)) (module-re-export! m (append-map module-exports (module-uses m))))))

  (define-syntax-rule (import-unexported module-name binding-name)
    ; imports and defines the binding locally at place.
    ; unquoted-module-name unquoted-binding-name ->
    (define binding-name (@@ module-name binding-name)))

  (define (environment* . name)
    "(symbol ...) ... -> environment/module
     similar to \"environment\" from (rnrs eval).
     this can be used to load modules that use syntax to create their module definition.
     the modules contents are first evaluated in the top-level environment - before the environment object is created.
     the syntax used for creating the module definition must be available in the current top-level environment.
     only the \".scm\" filename-extension is supported when resolving file paths from module names for loading"
    (map
      (l (a)
        (load
          (module-name->load-path-and-path a ".scm" %load-path (l (load-path full-path) full-path))))
      name)
    (apply environment name))

  ;-- files

  (define* (path->load-path path #:optional (load-path %load-path))
    "string -> false/path
     returns the first found load-path where path can be found.
     works for full and relative paths"
    (if (string-prefix? "/" path)
      (fold
        (l (a r)
          (let (a-full (realpath* a))
            (if (and a-full (string-prefix? a-full path))
              (if r (if (> (string-length a) (string-length r)) a r) a) r)))
        #f load-path)
      (any (l (a) (and (file-exists? (string-append (realpath* a) "/" path)) a)) load-path)))

  (define* (symbol-list->path a #:optional (filename-extension ".scm"))
    "(symbol ...) string -> string
     creates a filesystem path string from a module name. module existence is not checked.
     filename-extension can be false so that for example directory paths can be created"
    (let (a (string-join (map symbol->string a) "/"))
      (if filename-extension (string-append a filename-extension) a)))

  (define* (path->symbol-list a #:optional (filename-extension ".scm")) "string -> (symbol ...)"
    (map string->symbol
      (string-split
        (string-trim (if filename-extension (string-drop-suffix-if-exists filename-extension a) a)
          #\/)
        #\/)))

  (define (module-name->load-path-and-path a filename-extension load-path c)
    "(symbol ...) string (string ...) procedure:{string:load-path string:full-path -> any} -> any
     finds the load path under which a possibly partial (prefix) module name is saved.
     if no filename-extension is given it will usually only match directories"
    (let*
      ( (path (symbol-list->path a filename-extension))
        (result-load-path (path->load-path path load-path)))
      (and result-load-path (c result-load-path (string-append result-load-path "/" path)))))

  (define (module-match-rnrs-definition a)
    "any -> false/list:module-name
     matches a r6rs or r7rs library definition and returns the module name"
    (match a (((quote library) name _ ...) name) (((quote define-library name _ ...)) name) (_ #f)))

  (define (module-match-guile-definition a)
    "any -> false/list:module-name
     matches a guile \"define-module\" form and returns the module name"
    (match a (((quote define-module) ((? symbol? name) ...) _ ...) name) (_ #f)))

  (define (module-file->name a)
    "string -> (symbol ...)/false
     read file at the given path and, check the first expression for an r6rs or r7rs library definition
     and extract the module name"
    (module-match-rnrs-definition (call-with-input-file a read)))

  (define (module-file? path)
    "string -> boolean
     true if file contains as the first expression an r6rs or r7rs library definition"
    (if (module-file->name path) #t #f))

  (define*
    (module-find-one path #:key (load-path %load-path) ignore-content? guile-modules?
      file-content-match)
    "string [#:load-path (string ...) #:guile-modules? boolean #:ignore-content? boolean] -> false/(symbol ...):module-name
     setting the right load-path is important because the module name is derived from it.
     a file is considered a valid module if:
       it exists and is a regular file
       the file name extension is \".scm\"
       the file contains as the first expression an r6rs library or r7rs define-library form
       if \"guile-modules?\" is true: the file contains a define-module form
       it is in a load path and the module name matches the path under a load path (using %load-paths)"
    (and-let* ((path (realpath* path)))
      (and (string-suffix? ".scm" path)
        (and-let*
          ( (stat-info (false-if-exception (stat path)))
            (load-path-prefix (path->load-path path load-path)))
          (if ignore-content? (path->symbol-list (string-drop-prefix load-path-prefix path))
            (call-with-input-file path
              (l (file)
                (if file-content-match (file-content-match file)
                  (or (module-match-rnrs-definition (read file))
                    (and guile-modules?
                      (begin (seek file 0 SEEK_SET)
                        (let loop ((a (read file)))
                          (if (eof-object? a) #f
                            (or (module-match-guile-definition a) (loop (read file))))))))))))))))

  (define*
    (module-find path #:key (max-depth (inf)) (load-path %load-path) guile-modules? ignore-content?
      file-content-match)
    "string module-find-one-arguments ... -> ((module-name . path) ...)
     get all names for modules under or at path using module-find-one"
    (file-system-fold
      ; enter?
      (l (n s r) #t)
      ; leaf
      (l (n s r)
        (let
          (name
            (module-find-one n #:load-path
              load-path #:guile-modules?
              guile-modules? #:ignore-content?
              ignore-content? #:file-content-match file-content-match))
          (if name (pair (pair name n) r) r)))
      ; down
      (l (n s r) r)
      ; up
      (l (n s r) r)
      ; skip
      (l (n s r) r)
      ; error
      (l (n s errno r) r) (list) (remove-trailing-slash path)))

  (define (module-find-by-name name search-type load-paths)
    "(symbol ...) symbol:exact/prefix/prefix-not-exact (string ...) -> ((symbol ...):module-name ...)
     find module names by module name part"
    (let
      ( (search
          (l (name filename-extension) "list string/boolean -> list"
            (or
              (module-name->load-path-and-path name filename-extension
                load-paths
                (l (load-path full-path) "string string -> list"
                  (if (eq? (q regular) (stat:type (stat full-path))) (list name)
                    (map first
                      (module-find full-path #:load-path
                        (list load-path) #:file-content-match
                        (l (file)
                          (match (read file)
                            (((quote define-test-module) ((? symbol? name) ...) _ ...) name) (_ #f))))))))
              (list))))
        (filename-extensions
          (append
            (if (or (eqv? (q prefix) search-type) (eqv? (q prefix-not-exact) search-type))
              (list #f) (list))
            (if (or (eqv? (q prefix) search-type) (eqv? (q exact) search-type)) (list ".scm")
              (list)))))
      (append-map (l (a) (search name a)) filename-extensions)))

  ;-- other

  (define-syntax include-from-load-path
    (lambda (s)
      "literal-string -> expressions
       include contents of a scheme source file, searching for filename in the current load-path.
       similar to guiles include-from-path"
      (syntax-case s ()
        ( (_ filename)
          (let ((path (%search-load-path (syntax->datum (syntax filename)))))
            (quasisyntax (unsyntax (datum->syntax s (pair (quote begin) (file->datums path))))))))))

  (define (load-with-environment path env)
    "string module ->
     load filename and evaluate its contents with the given eval environment which may be a module, a r6rs library or a environment"
    (let (port (open-file path "r"))
      (let loop ((expr (read port)) (r #f))
        (if (eof-object? expr) r (loop (read port) (eval expr env))))
      (close port))))
