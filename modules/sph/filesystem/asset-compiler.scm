(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-config-input
    ac-config-valid?
    ac-input-copy
    ac-output-copy
    ac-source-files-updated?
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

  (define-record ac-config-input name path? processor)

  (define (config-output->config-input config-output path)
    (find (l (a) ((ac-config-input-path? a) path)) (tail config-output)))

  (define (config-output->output-processor a mode)
    "gets the output processor from a config-output configuration"
    (or (and (list? a) (hashtable-ref (first a) mode)) ac-output-copy))

  (define (source->input-processor config-output processor-config a)
    (let*
      ((a (any->list-s a)) (config-input (config-output->config-input config-output (first a))))
      (if config-input
        (l (port) ((ac-config-input-processor config-input) processor-config a port))
        (l (port) (ac-input-copy processor-config a port)))))

  (define (ac-input-copy config sources port) (each (l (a) (file->port a port)) sources))
  (define (ac-output-copy config sources port) (each (l (a) (a port)) sources))

  (define (ac-destination path-directory format path-file input-spec)
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

  (define (ac-source-files-updated? path-destination paths-input)
    "string (string ...) -> boolean
     * destination does not exist: true
     * destination path older than any input file: true"
    (if (file-exists? path-destination)
      (let
        ( (paths-input-mtime (apply max (map (l (a) (stat:mtime (stat a))) paths-input)))
          (path-destination-mtime (stat:mtime (stat path-destination))))
        (> paths-input-mtime path-destination-mtime))
      #t))

  (define (ac-config-valid? a)
    "any -> boolean
     true if input is a valid config object"
    (and (hashtable? a)
      (apply-values
        (l (keys values)
          (let (keys (vector->list keys))
            (or (null? keys)
              (and (every symbol? keys)
                (every
                  (l (a)
                    (and (list? a) (= 2 (length a)) (hashtable? (first a)) (vector? (second a))))
                  (vector->list values))))))
        (hashtable-entries a))))

  (define* (ac-compile config mode output-port output-format sources #:optional processor-config)
    "hashtable symbol port symbol (string ...) (string/list ...) ->
     maps all sources via input processors to a list and passes it to an output processor that writes to output-port.
     \"mode\" is for example development or production.
     data structures:
     * config: hashtable:{format-name -> }
     * config-format: (hashtable:config-output vector:config-input ...)
     * config-output: hashtable:{mode -> processor}
     * config-input: vector:(symbol:name procedure:{string:path -> boolean} procedure:processor)
     * mode: symbol
     * processor: procedure:{string/(string ...):sources port:port-output ->}
     * sources: (any:processor-dependent ...)
     example config:
     (define client-ac-config
       (symbol-hashtable
         javascript
         (list
           (symbol-hashtable production javascript-output-compress development javascript-output-format)
           (record ac-config-input (q sescript) (has-suffix-proc \".sjs\") s-template-sescript->javascript))
         html
         (list
           (symbol-hashtable)
           (record ac-config-input \"sxml\" (has-suffix-proc \".sxml\") s-template-sxml->html))))"
    (let (config-format (hashtable-ref config output-format))
      ( (config-output->output-processor config-format mode) processor-config
        (map (l (a) (source->input-processor config-format processor-config a)) sources) output-port)))

  (define*
    (ac-compile->file config mode dest-directory output-format sources #:key processor-config
      only-if-newer
      dest-file-name)
    "-> string:path-destination
     if \"only-if-newer\" is true, checks the modification times of files and only copies if any source is newer than the destination.
     \"dest-file-name\" sets the destination file name to use instead of an automatically generated one"
    (let*
      ( (sources-flat (flatten sources))
        (path-destination (ac-destination dest-directory output-format dest-file-name sources-flat)))
      (if
        (or (not only-if-newer)
          (and (every string? sources-flat)
            (ac-source-files-updated? path-destination sources-flat)))
        (and (ensure-directory-structure (dirname path-destination))
          (call-with-output-file path-destination
            (l (port) (ac-compile config mode port output-format sources processor-config)))
          path-destination)
        path-destination))))
