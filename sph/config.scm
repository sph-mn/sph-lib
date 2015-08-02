(library (sph config)
  (export
    config-clear!
    config-load
    config-loaders
    config-ref
    config-save
    config-savers
    config-set!
    primitive-config-ref
    primitive-config-set!
    sph-config-object)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph hashtable one)
    (sph one)
    (only (sph read-write) file->datums)
    (sph tree)
    (except (rnrs hashtables) hashtable-ref)
    (only (sph filesystem) ensure-trailing-slash))

  ;persisted program configuration loaded into a configuration object.

  (define (parse-config-file path)
    (tree-map-lists-and-self (compose alist->hashtable list->alist)
      (primitive-eval (list (q quasiquote) (file->datums path)))))

  (define sph-config-object (symbol-hashtable))

  (define (config-load-default-get-path path name)
    "string/false string -> string
    create a configuration-file path.
    path + \"config/\" + name + \".scm\""
    (string-append
      (if path (ensure-trailing-slash path)
        (string-append (ensure-trailing-slash (getcwd)) "config/"))
      name ".scm"))

  (define (config-save-default-get-path path name)
    (string-append (string-drop-right (config-load-default-get-path path name) 4) ".runtime.scm"))

  (define (config-load-default name options)
    "string symbol-alist -> config-object
    the default config-loader.
    loads config from a file either from a path given as an element in options like (symbol . string) or
    in a directory named \"config\" in the current working directory.
    the module-local \"config\" variable is updated."
    (let (path (config-load-default-get-path (alist-ref options (q path)) name))
      (if (file-exists? path) (parse-config-file path)
        (throw (q configuration-file-does-not-exist) path))))

  (define (config-save-default config options)
    (call-with-output-file
      (config-save-default-get-path (alist-ref options (q path)) (hashtable-ref config (q name)))
      (l (port) (write (hashtable->alist config 32) port))))

  (define-as config-loaders symbol-hashtable default config-load-default)
  (define-as config-savers symbol-hashtable default config-save-default)

  (define* (config-load #:optional name/config (loader-key (q default)) loader-options)
    "symbol/hashtable:name/config [alist] -> config-object
    successive calls to this procedure update the global configuration by merging.
    if name/config is a hashtable, the is is merged into the current \"config\" object,
    if name is a true value it is passed to a config loader selected by loader-key.
    the default config loader reads a \"flat-alist-tree\" from a file.
    if name/config is false, look for an environment variable sph-config-name and try to read a file $sph-config-name/default.scm"
    (if (hashtable? name/config) (hashtable-tree-merge! sph-config-object name/config)
      (let (name (or name/config (getenv "sph-config-name") "default"))
        (hashtable-tree-merge! sph-config-object
          ((hashtable-ref config-loaders loader-key) name loader-options))
        (hashtable-set! sph-config-object (q config-name) name)))
    sph-config-object)

  (define*
    (config-save #:optional (saver-key (q default)) saver-options (config sph-config-object))
    ((hashtable-ref config-savers saver-key) config saver-options))

  (define (config-clear! name/config) (set! sph-config-object (symbol-hashtable)))

  (define-syntax-rule (primitive-config-set! symbol ... value)
    (hashtables-set! sph-config-object symbol ... value))

  (define-syntax-rule (primitive-config-ref symbol ...)
    (hashtables-ref sph-config-object symbol ...))

  (define-syntax-rule (config-set! unquoted-symbol ... value)
    (primitive-config-set! (q unquoted-symbol) ... value))

  (define-syntax-rule (config-ref unquoted-symbol ...)
    (primitive-config-ref (q unquoted-symbol) ...)))