(library (sph lang docl)
  (export
    call-with-docl
    docl-default-env-module-names
    docl-include-stack
    docl-translate-any
    docl-translate-port)
  (import
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph lang docl env)
    (only (guile) port-filename catch)
    (only (rnrs hashtables) equal-hash)
    (only (sph list) contains?)
    (only (sph one) first-as-result)
    (only (sph two) define-stack-fluid))

  (define-stack-fluid docl-include-stack)
  (define docl-default-env-module-names (q ((rnrs base) (guile) (sph lang docl env) (sph lang docl env default))))

  (define
    (call-with-docl get-source-identifier get-source-position bindings keep-prev-bindings exception-keys input
      proc)
    "{any:input -> any} {input -> any} hashtable-quoted/boolean boolean boolean/(symbol ..) any:input {input -> any} -> any/string
    installs a handler that amends exceptions with source information so that an exception-handler
    receives the arguments (key source-name source-position other-exception-arguments ...) for exceptions matching exception-keys.
    sets up the circular inclusion protection and bindings which are accessible with
    the procedures from (sph lang docl env). then calls \"proc\" with \"input\""
    (let (source-name (get-source-identifier input))
      (if (and source-name (contains? (docl-include-stack) source-name)) ""
        (begin (docl-include-stack-add source-name)
          (catch exception-keys
            (l () (if (not keep-prev-bindings) (docl-env-reset!))
              (if bindings (docl-env-update! bindings))
              (first-as-result (proc input) (docl-include-stack-remove)))
            (l (key . args) (docl-include-stack-remove)
              (apply throw key
                source-name (and get-source-position (get-source-position input))
                args)))))))

  (define* (docl-translate-any input proc #:optional bindings keep-prev-bindings)
    "calls proc with input and enables docl features as with call-with-docl.
    input can be anything, and the source-position argument for exceptions will be false."
    (call-with-docl equal-hash #f bindings keep-prev-bindings #t input proc))

  (define* (docl-translate-port input proc #:optional bindings keep-prev-bindings)
    "port procedure:{port -> any} hashtable-quoted/boolean boolean -> any
    calls proc with input and enables docl features as with call-with-docl.
    input must be a port, and the source-identifier and source-position arguments for exceptions are set using port-filename and port-position"
    (call-with-docl port-filename port-position bindings keep-prev-bindings #t input proc)))
