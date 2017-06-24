(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-config-input
    ac-config-input-name
    ac-config-input-path-matcher
    ac-config-input-processor
    ac-config-valid?
    ac-destination
    ac-input-copy
    ac-output-copy
    ac-source-files-updated?
    sph-filesystem-asset-compiler-description)
  (import
    (guile)
    (sph base)
    (sph record))

  (define sph-filesystem-asset-compiler-description
    "process and merge files of various formats into one.
     example use case: mix of transcompiled and non-compiled files that are merged and optimised for the web")

  (define-record ac-config-input name path-matcher processor)

  (define (config-format->config-input config-format path)
    "list string -> false/vector
     find a matching input processor for path using ac-config-input-path-matcher procedures on path until one matches"
    (find (l (a) ((ac-config-input-path-matcher a) path)) (tail config-format)))

  (define (source->input-processor a config-format processor-options)
    "string/(string ...) hashtable any -> procedure
     find a matching config-input and return an input-processor procedure"
    (let* ((a (any->list a)) (config-input (config-format->config-input config-format (first a))))
      (if config-input
        (l (out-port) ((ac-config-input-processor config-input) a out-port processor-options))
        (l (out-port) (ac-input-copy a out-port processor-options)))))

  (define (ac-input-copy sources port options)
    "list port any -> unspecified
     a default processor for just reading all source files and writing their content to port"
    (files->port sources port))

  (define (ac-output-copy input-processors port options)
    "(procedure ...) port any -> unspecified
     a default processur for calling all given input processors with output port"
    (each (l (a) (a port)) input-processors))

  (define (config-format->output-processor a mode)
    "pair symbol/any -> false/procedure
     gets the output processor from a config-format configuration"
    (hashtable-ref (first a) mode ac-output-copy))

  (define* (ac-destination path-directory format sources #:optional path-file)
    "string symbol string list -> string
     create a string for an output path relative to \"path-directory\".
     format:
     * \"{path-directory}/{format}/{path-file}\"
     * \"{path-directory}/{format}/_{basename-without-suffix-first-of-sources}-{base32-hash-of-sources}\""
    (string-append (ensure-trailing-slash path-directory) (symbol->string format)
      "/"
      (if path-file path-file
        (string-append "_" (first (string-split (basename (first sources)) #\.))
          "-" (number->string (equal-hash sources) 32)))))

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
                    (and (list? a) (>= 2 (length a))
                      (hashtable? (first a)) (every vector? (tail a))))
                  (vector->list values))))))
        (hashtable-entries a))))

  (define* (ac-compile config mode port output-format sources #:optional processor-options)
    "hashtable symbol port symbol (string ...) (string/list ...) ->
     maps all sources to input processors passes those to an output processor that writes to port.
     \"mode\" is for example development or production.
     \"processor-options\" is an optional custom value passed to input processors and the output processor as the last argument.
     note that a config-format can contain multiple config-input, for multiple source formats of a target format.
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
         (pair
           (symbol-hashtable production javascript-output-compress development javascript-output-format)
           (record ac-config-input (q sescript) (has-suffix-proc \".sjs\") s-template-sescript->javascript))
         html
         (pair
           (symbol-hashtable)
           (record ac-config-input \"sxml\" (has-suffix-proc \".sxml\") s-template-sxml->html))))"
    (and-let*
      ( (config-format (hashtable-ref config output-format))
        (output-processor (config-format->output-processor config-format mode)))
      (output-processor
        (map (l (a) (source->input-processor a config-format processor-options)) sources) port
        processor-options)))

  (define*
    (ac-compile->file config mode dest-directory output-format sources #:key processor-options
      only-if-newer
      dest-file-name)
    "-> string:path-destination
     if \"#:only-if-newer\" is true, checks the modification times of files and only copies if any source is newer than the destination.
     \"#:dest-file-name\" sets the destination file name to use instead of an automatically generated one"
    (let*
      ( (sources-flat (flatten sources))
        (path-destination (ac-destination dest-directory output-format sources-flat dest-file-name)))
      (if
        (or (not only-if-newer)
          (and (every string? sources-flat)
            (ac-source-files-updated? path-destination sources-flat)))
        (and (ensure-directory-structure (dirname path-destination))
          (call-with-output-file path-destination
            (l (port) (ac-compile config mode port output-format sources processor-options)))
          path-destination)
        path-destination))))
