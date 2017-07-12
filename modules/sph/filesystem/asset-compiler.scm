(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-config-input
    ac-config-input-match?
    ac-config-input-name
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
    "configuration format and helpers to copy or compile and process data from custom sources.
     for example to compile from many files in different preprocessor formats into one target format file.
     # data structures
     * config: hashtable:{symbol:format-name -> list}
     * config-format: (config-output config-input ...)
     * config-output: hashtable:{symbol:mode -> procedure:processor}
     * config-input: #(symbol:name procedure:source-matches? procedure:processor)
     * source-matches? :: any:path -> boolean
     * processor :: sources port:output any:processor-options ->
     * sources: (any:processor-dependent ...)
     example config:
     (define client-ac-config
       (ht-create-symbol
         javascript
           (list
             (ht-create-symbol production javascript-output-compress development javascript-output-format)
             (record ac-config-input (q sescript) source-matches? s-template-sescript->javascript))
         html
           (list
             (ht-create-symbol)
             (record ac-config-input (q sxml) (has-suffix-proc \".sxml\") s-template-sxml->html)
             (record ac-config-input (q html) (has-suffix-proc \".html\") ac-input-copy))))")

  (define-record input-config name match? processor)

  (define (ac-config-valid? a)
    "any -> boolean
     true if input is a valid config object"
    (and (ht? a)
      (apply-values
        (l (keys values)
          (let (keys (vector->list keys))
            (or (null? keys)
              (and
                ; output formats
                (every symbol? keys)
                ; processors
                (every list? values)
                (every
                  (l (a)
                    (and (not (null? a))
                      ; output processor
                      (ht? (first a))
                      ; input processors
                      (every
                        (l (a)
                          (and (vector? a) (= 3 (vector-length a))
                            (symbol? (vector-ref a 0)) (procedure? (vector-ref a 1))
                            (procedure? (vector-ref a 2))))
                        (tail a))))
                  values)))))
        (ht-entries a))))

  (define (ac-compile config output-format mode sources port #:optional processor-options)
    "hashtable symbol port symbol (string ...) (string/list ...) ->
     \"processor-options\" is an optional value passed to input processors and the output processor as the last argument"
    (and-let*
      ( (processors (ht-ref config output-format))
        (output-processor (ht-ref (first processors) mode)) (input-processors (tail processors))
        (input-processors
          ; map sources to procedures that process them
          (map
            (l (a)
              (or
                (any
                  (l (b)
                    (let (source-match? (vector-ref b 1))
                      (if (source-match? a)
                        (let (processor (vector-ref b 2))
                          (l (out-port) (processor a out-port options processor-options))))))
                  input-processors)
                (raise (q no-matching-input-processor))))
            sources)))
      (output-processor input-processors port processor-options)))

  ;-- filesystem

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
          "-" (number->string (ht-hash-equal sources) 32)))))

  (define (ac-source-files-updated? dest sources)
    "string (string ...) -> boolean
     true if any source is newer than destination"
    (let (dest-mtime (stat:mtime (stat dest)))
      (any (l (a) (< dest-mtime (stat:mtime (stat a)))) sources)))

  (define*
    (ac-compile->file config output-format mode sources dest-directory #:key processor-options when
      dest-name)
    "hashtable symbol string symbol list -> string:path-destination
     #:when takes a symbol:
     * new: update only if destination does not exist
     * newer: update if destination does not exist or any source file is newer
     * always: always compile, overwriting any existing destination file
     \"#:dest-name\" sets the destination file name to use instead of an automatically generated one"
    (let*
      ( (sources-flat (flatten sources))
        (path-destination (ac-destination dest-directory output-format sources-flat dest-file-name)))
      (if
        (or (eq? (q always) when)
          (not (or (file-exists? path-destination) (every string? sources-flat)))
          (ac-source-files-updated? path-destination sources-flat))
        (and (ensure-directory-structure (dirname path-destination))
          (call-with-output-file path-destination
            (l (port) (ac-compile config output-format mode sources port processor-options)))
          path-destination)
        path-destination)))

  #;(define (ac-output-copy input-processors port options)
    "(procedure ...) port any -> unspecified
     a default processur for calling all given input processors with output port"
    (each (l (a) (a port)) input-processors))

  #;(define (ac-input-copy sources port options)
    "(string:file-path/any ...) port any -> unspecified
     a default processor that interprets strings in sources as
     file paths and writes the file contents to port, and everything else using scheme write"
    (each (l (a) (if (string? a) (file->port a port) (write a port))) sources)))
