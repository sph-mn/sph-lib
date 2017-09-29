(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-config
    ac-config-input
    ac-config-input-p
    ac-config-output
    ac-config-output-p
    ac-config-p
    ac-config-valid?
    ac-destination
    ac-input-id
    ac-input-matcher
    ac-input-processor
    ac-input-record
    ac-output-id
    ac-output-processor
    ac-output-record
    ac-output-suffix
    ac-source-files-updated?
    sph-filesystem-asset-compiler-description)
  (import
    (guile)
    (sph base)
    (sph record))

  (define sph-filesystem-asset-compiler-description
    "configuration format and helpers to concatenate/preprocess code from multiple sources.
     for example to compile from many files in different preprocessor formats into one target format file.
     data structures
       config: hashtable:{symbol:output-format -> (config-output config-input ...)}
       config-output: #(symbol:id string:suffix false/procedure:processor-output)
       config-input: #(symbol:name source-match? false/procedure:processor-input)
       config-format: (config-output config-input ...)
       source-match? :: boolean/procedure:{any:source -> boolean}
       processor-input :: source-element port:output any:processor-options ->
       processor-output :: procedure:input->port port:output-port any:processor-options ->
       sources: (any:processor-dependent ...)
     notes
       config-output config-input
       source-match?

     syntax
       ac-config :: (out-id suffix out-proc) (in-id matcher in-proc) ... -> hashtable
       ac-config-input :: id matcher processor -> vector
       ac-config-output :: id suffix processor -> vector
     example config
       (define my-ac-config
         (ac-config
           ( (html \".html\" (l (process-input out-port options) (process-input out-port)))
             (html (q ext) (l (source out-port options) (file->port source out-port)))
             (sxml #t s-template-sxml->html))
           ( (js (q ext) js-output-processor)
             (js (q ext) ac-input-copy)
             (sjs (q ext) s-template-sescript->js))))")

  (define-syntax-rule (ac-config ((out-id suffix out-proc) (in-id matcher in-proc) ...) ...)
    (ac-config-p
      (list
        (list (q out-id) (ac-config-output out-id suffix out-proc)
          (ac-config-input in-id matcher in-proc) ...)
        ...)))

  (define-syntax-rule (ac-config-output id suffix processor)
    (ac-config-output-p (q id) suffix processor))

  (define-syntax-rule (ac-config-input id matcher processor)
    (ac-config-input-p (q id) matcher processor))

  (define-record ac-input id matcher processor)
  (define-record ac-output id suffix processor)
  (define ac-input-record ac-input)
  (define ac-output-record ac-output)
  (define (id->extension a) (string-append "." (symbol->string a)))
  (define (match-suffix-f suffix) (l (a) (and (string? a) (string-suffix? suffix a))))

  (define (ac-config-p config-source)
    "list:((id config-output config-input ...) ...) -> hashtable
     create an ac-config from a list with the following format"
    (ht-from-alist config-source))

  (define (ac-config-output-p id suffix processor)
    "symbol string/procedure/boolean procedure -> vector
     create a config-output object.
     if processor is false, a default processor that just copies is used.
     suffix
       #t: use the id as a filename extension separated with a dot
       else: no filename extension"
    (vector id
      (cond ((and (boolean? suffix) suffix) (id->extension id)) ((string? suffix) suffix) (else ""))
      processor))

  (define (ac-config-input-p id matcher processor)
    "symbol procedure/boolean procedure/false
     create a config-input object.
     if processor is false, a default processor that interprets source elements as
     filesystem paths and reads them is used.
     matcher
       procedure: match a source element
       #t: match the id as a filename extension separated with a dot
           for source elements which are strings
       else: never match"
    (vector id
      (if (procedure? matcher) matcher
        (if (boolean? matcher) (if matcher (match-suffix-f (id->extension id)) (const #f))
          (const #f)))
      processor))

  (define (ac-input-copy source port options)
    "a default processor-input that interprets source as a file name if it is a string.
     copies all contents of the source file to port with a newline at the end.
     false and does nothing otherwise"
    (and (string? source) (begin (file->port source port) (newline port))))

  (define (ac-output-copy get-input port options)
    "a default processor-output that copies input to output port" (get-input port))

  (define ac-compile
    (let*
      ( (sources->get-and-argument
          (l (sources config-input-list)
            "list vector -> ((procedure:processor . source) ...)
            find a matching processor/reader for each source or raise an exception if no processor matches"
            (map
              (l (a)
                (or
                  (any
                    (l (b)
                      (and ((ac-input-matcher b) a)
                        (pair (or (ac-input-processor b) ac-input-copy) a)))
                    config-input-list)
                  (raise (list (q no-matching-input-processor) a))))
              sources)))
        (sources->input-processor
          (l (sources config-input options)
            "list list any -> procedure
             create a procedure that processes all source elements with a matching config-input.
             note: with some output formats you may want to emit a newline or similar after each source in the input processor,
             to prevent for example code being appended to a line comment from the last line of the previous file"
            (let (get-and-argument (sources->get-and-argument sources config-input))
              (l (out-port) (each (l (a) ((first a) (tail a) out-port options)) get-and-argument))))))
      (l* (config output-format sources port #:optional processor-options)
        "hashtable symbol list port [any] ->
        \"processor-options\" is an optional value passed to input and output processors as the last argument"
        (and-let* ((config-format (ht-ref config output-format)))
          ( (or (ac-output-processor (first config-format)) ac-output-copy)
            (sources->input-processor sources (tail config-format) processor-options) port
            processor-options)))))

  ;-- filesystem

  (define* (ac-destination path-directory format sources #:optional name suffix)
    "string symbol string list -> string
     create a string for an output path relative to \"path-directory\".
     format:
     * \"{path-directory}/{format}/{name}\"
     * \"{path-directory}/{format}/_{basename-without-suffix-first-of-sources}-{base32-hash-of-sources}\""
    (string-append (ensure-trailing-slash path-directory) (symbol->string format)
      "/"
      (if name (string-append name suffix)
        (let (strings (filter string? sources))
          (string-append "_"
            (if (null? strings) "_" (first (string-split (basename (first strings)) #\.))) "-"
            (number->string (ht-hash-equal sources) 32) suffix)))))

  (define (ac-source-files-updated? dest sources)
    "string (string ...) -> boolean
     true if any source is newer than destination"
    (let (dest-mtime (stat:mtime (stat dest)))
      (any (l (a) (< dest-mtime (stat:mtime (stat a)))) sources)))

  (define*
    (ac-compile->file config output-format sources dest-directory #:key processor-options when
      dest-name)
    "hashtable symbol list string _ ... -> string:path-destination
     #:processor-options: any/alist
     #:when takes a symbol:
       new: update only if destination does not exist
       newer: update if destination does not exist or any source file is newer
       always: always compile, overwriting any existing destination file
     #:dest-name sets the destination file name to use instead of an automatically generated one"
    (let*
      ( (config-format (ht-ref config output-format)) (config-output (first config-format))
        (sources-flat (flatten sources))
        (path-destination
          (ac-destination dest-directory output-format
            sources-flat dest-name (ac-output-suffix config-output))))
      (if
        (or #t (eq? (q always) when)
          (not (every string? sources-flat)) (not (file-exists? path-destination))
          (ac-source-files-updated? path-destination sources-flat))
        (and (ensure-directory-structure (dirname path-destination))
          (call-with-output-file path-destination
            (l (port) (ac-compile config output-format sources port processor-options)))
          path-destination)
        path-destination))))
