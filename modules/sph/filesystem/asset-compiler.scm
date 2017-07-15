(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-config-valid?
    ac-destination
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
     * config: hashtable:{symbol:format-name -> (output-processor-config input-processor-config ...)}
     * output-processor-config: false/procedure:output-processor
     * input-processor-config: #(symbol:name procedure:source-matches? procedure:input-processor)
     * source-matches? :: any -> boolean
     * input-processor :: source port:output any:processor-options ->
     * output-processor :: procedure:process-input port:output-port any:processor-options ->
     * sources: (any:processor-dependent ...)
     example config:
     (define client-ac-config
       (ht-create-symbol
         javascript
         (list javascript-output-compress
           (vector (q sescript) source-matches? s-template-sescript->javascript))
         html
         (list #f
           (vector (q sxml) (has-suffix-proc \".sxml\") s-template-sxml->html)
           (vector (q html) (has-suffix-proc \".html\") (l (source out-port options) (file->port source out-port))))))")

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
                      (or (not (first a)) (ht? (first a)))
                      ; input processors
                      (every
                        (l (a)
                          (and (vector? a) (= 3 (vector-length a))
                            (symbol? (vector-ref a 0)) (procedure? (vector-ref a 1))
                            (procedure? (vector-ref a 2))))
                        (tail a))))
                  values)))))
        (ht-entries a))))

  (define ac-compile
    (let*
      ( (sources->proc-and-argument
          (l (sources input-processors-config)
            "list (#(name source-match? processor) ...) -> ((procedure:processor . any:source) ...)
            get a matching processor for each source or raise an exception if no processor matches"
            (map
              (l (a)
                (or
                  (any (l (b) (and ((vector-ref b 1) a) (pair (vector-ref b 2) a)))
                    input-processors-config)
                  (raise (list (q no-matching-input-processor) a))))
              sources)))
        (sources->input-processor
          (l (sources input-processors-config options)
            "list list any -> procedure
            create a procedure that processes each source"
            (let (sources (sources->proc-and-argument sources input-processors-config))
              (l (out-port) (each (l (a) ((first a) (tail a) out-port options)) sources))))))
      (l* (config output-format sources port #:optional processor-options)
        "hashtable symbol port symbol (string ...) (string/list ...) ->
        \"processor-options\" is an optional value passed to input processors and the output processor as the last argument"
        (and-let* ((processors (ht-ref config output-format)))
          ( (or (first processors) (l (process-input out-port options) (process-input out-port)))
            (sources->input-processor sources (tail processors) processor-options) port
            processor-options)))))

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
        (let (strings (filter string? sources))
          (string-append "_"
            (if (null? strings) "_" (first (string-split (basename (first strings)) #\.))) "-"
            (number->string (ht-hash-equal sources) 32))))))

  (define (ac-source-files-updated? dest sources)
    "string (string ...) -> boolean
     true if any source is newer than destination"
    (let (dest-mtime (stat:mtime (stat dest)))
      (any (l (a) (< dest-mtime (stat:mtime (stat a)))) sources)))

  (define*
    (ac-compile->file config output-format sources dest-directory #:key processor-options when
      dest-name)
    "hashtable symbol string symbol list -> string:path-destination
     #:when takes a symbol:
     * new: update only if destination does not exist
     * newer: update if destination does not exist or any source file is newer
     * always: always compile, overwriting any existing destination file
     \"#:dest-name\" sets the destination file name to use instead of an automatically generated one"
    (let*
      ( (sources-flat (flatten sources))
        (path-destination (ac-destination dest-directory output-format sources-flat dest-name)))
      (if
        (or (eq? (q always) when) (not (every string? sources-flat))
          (not (file-exists? path-destination))
          (ac-source-files-updated? path-destination sources-flat))
        (and (ensure-directory-structure (dirname path-destination))
          (call-with-output-file path-destination
            (l (port) (ac-compile config output-format sources port processor-options)))
          path-destination)
        path-destination))))
