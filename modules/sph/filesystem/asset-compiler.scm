(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-input-copy
    ac-lang-input
    ac-output-copy
    sph-filesystem-asset-compiler-description)
  (import
    (guile)
    (sph base)
    (sph record)
    (only (sph tree) flatten))

  (define sph-filesystem-asset-compiler-description
    "process and merge files of various formats into one.
     uses a configuration for processing custom formats.
     uses custom load-paths and can automatically create target paths.
     example use case: mix of transcompiled and non-compiled files that are merged and optimised for the web")

  (define-record ac-lang-input name path? processor)

  (define (create-output-path path-directory format path-file input-spec)
    "string symbol string list -> string
     create a string for an output path relative to \"path-directory\".
     format:
     * \"{path-directory}/{format}/{path-file}\"
     * \"{path-directory}/{format}/_{basename-without-suffix-first-of-input-spec}-{base32-hash-of-input-spec}\""
    (string-append (ensure-trailing-slash path-directory) (symbol->string format)
      "/"
      (if path-file path-file
        (string-append "_" (first (string-split (basename (first input-spec)) #\.))
          "-" (number->string (equal-hash input-spec) 32)))))

  (define (input-files-updated? path-destination paths-input)
    "string (string ...) -> boolean
     * destination does not exist: true
     * destination path older than any input file: true"
    (if (file-exists? path-destination)
      (let
        ( (paths-input-mtime (apply max (map (l (a) (stat:mtime (stat a))) paths-input)))
          (path-destination-mtime (stat:mtime (stat path-destination))))
        (> paths-input-mtime path-destination-mtime))
      #t))

  (define (lang-output->lang-input lang-output path)
    (find (l (a) ((ac-lang-input-path? a) path)) (tail lang-output)))

  (define (ac-input-copy config sources port) (each (l (a) (file->port a port)) sources))
  (define (ac-output-copy config sources port) (each (l (a) (a port)) sources))

  (define (source->input-processor lang-output processor-config a)
    (let* ((a (any->list-s a)) (lang-input (lang-output->lang-input lang-output (first a))))
      (if lang-input (l (port) ((ac-lang-input-processor lang-input) processor-config a port))
        (l (port) (ac-input-copy processor-config a port)))))

  (define (lang-output->output-processor a mode)
    "gets the output processor from a lang-output configuration"
    (or (and (list? a) (hashtable-ref (first a) mode)) ac-output-copy))

  (define (ac-config-lang-valid? a)
    "any -> boolean
     true if input is a valid config-lang object"
    (and (hashtable? a)
      (let (keys (vector->list (hashtable-keys a))) (or (null? keys) (every symbol? keys)))))

  (define*
    (ac-compile config-lang mode port-output output-format input-spec #:optional processor-config)
    "hashtable symbol port symbol (string ...) (string/list ...) ->
     compile to port. mode is for example development or production.
     data structures:
     * config-lang: hashtable:{format-name -> (hashtable:{mode -> processor} vector:ac-lang-input ...)}
     * ac-lang-input: vector:(symbol:name procedure:{string:path -> boolean} procedure:processor)
     * mode: symbol/key-in-output-format-processors
     * processor: procedure:{string/list:sources port ->}
     * input-spec: (string/input-spec:processor-dependent ...)
     example config-lang:
       (define client-ac-config
     (symbol-hashtable
       javascript
       (list
     (symbol-hashtable production javascript-output-compress development javascript-output-format)
     (record ac-lang-input (q sescript) (has-suffix-proc \".sjs\") s-template-sescript->javascript))
       html
       (list (symbol-hashtable)
     (record ac-lang-input \"sxml\" (has-suffix-proc \".sxml\") s-template-sxml->html))))"
    (let (lang-output (hashtable-ref config-lang output-format))
      ( (lang-output->output-processor lang-output mode) processor-config
        (map (l (a) (source->input-processor lang-output processor-config a)) input-spec) port-output)))

  (define*
    (ac-compile->file config-lang mode output-directory output-format input-spec #:key
      processor-config
      only-if-newer
      output-file-name)
    "-> string:path-destination"
    (let*
      ( (input-spec-flat (flatten input-spec))
        (path-destination
          (create-output-path output-directory output-format output-file-name input-spec-flat)))
      (if
        (or (not only-if-newer)
          (and (every string? input-spec-flat)
            (input-files-updated? path-destination input-spec-flat)))
        (and (ensure-directory-structure (dirname path-destination))
          (call-with-output-file path-destination
            (l (port) (ac-compile config-lang mode port output-format input-spec processor-config)))
          path-destination)
        path-destination))))
