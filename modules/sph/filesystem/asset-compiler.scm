(library (sph filesystem asset-compiler)
  (export
    ac-compile
    ac-compile->file
    ac-input-copy
    ac-lang-input
    ac-output-copy)
  (import
    (sph common)
    (sph record)
    (only (rnrs hashtables) equal-hash))

  ;merges and processes files of various formats into one.
  ;uses a configuration for processing different formats.
  ;uses custom load-paths and can automatically create a target path.
  ;example use case: mix of transcompiled and non-compiled files that are merged and optimised for the web
  (define-record ac-lang-input name path? processor)

  (define (create-output-path path-directory format path-file input-spec)
    (string-append (ensure-trailing-slash path-directory) (symbol->string format)
      "/"
      (if path-file path-file
        (string-append "_" (first (string-split (basename (first input-spec)) #\.))
          "-" (number->string (equal-hash input-spec) 32)))))

  (define (lang-output->lang-input lang-output path)
    (find (l (a) ((ac-lang-input-path? a) path)) (tail lang-output)))

  (define (input-files-updated? path-destination paths-input) "string (string ...) -> boolean"
    (if (file-exists? path-destination)
      (let
        ( (paths-input-mtime (apply max (map (l (a) (stat:mtime (stat a))) paths-input)))
          (path-destination-mtime (stat:mtime (stat path-destination))))
        (> paths-input-mtime path-destination-mtime))
      #t))

  (define (ac-input-copy config sources port) (each (l (a) (file->port a port)) sources))
  (define (ac-output-copy config sources port) (each (l (a) (a port)) sources))

  (define (source->input-processor lang-output processor-config a)
    (let* ((a (any->list-s a)) (lang-input (lang-output->lang-input lang-output (first a))))
      (if lang-input (l (port) ((ac-lang-input-processor lang-input) processor-config a port))
        (l (port) (ac-input-copy processor-config a port)))))

  (define (lang-output->output-processor a mode)
    (or (and (list? a) (hashtable-ref (first a) mode)) ac-output-copy))

  (define*
    (ac-compile config-lang mode port-output output-format input-spec #:optional processor-config)
    "hashtable symbol port symbol (string ...) (string/list ...) ->
    config-lang: hashtable:{format-name -> (hashtable:{mode -> processor} vector:ac-lang-input ...)}
    ac-lang-input: vector:(symbol:name procedure:{string:path -> boolean} procedure:processor)
    mode: symbol/key-in-output-format-processors
    processor: procedure:{string/list:sources port ->}
    input-spec: (string/input-spec:processor-dependent ...)"
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
            (l (port)
              (process-chain-finished-successfully?
                (ac-compile config-lang mode port output-format input-spec processor-config))))
          path-destination)
        path-destination))))
