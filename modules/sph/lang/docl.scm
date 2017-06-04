(library (sph lang docl)
  (export
    call-with-docl
    docl-default-env-module-names
    docl-include-stack
    docl-state-add-parameters
    docl-state-empty
    docl-translate-any
    docl-translate-port
    sph-lang-docl-description)
  (import
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph error)
    (only (guile)
      current-thread
      port-filename
      catch)
    (only (rnrs hashtables) equal-hash)
    (only (sph alist) list->alist)
    (only (sph list) contains?)
    (only (sph one) begin-first))

  (define sph-lang-docl-description
    "evaluate templates with a specific scheme environment, state values and circular inclusion protection")

  (define docl-default-env-module-names (q ((sph lang docl env default))))
  ;(call-hierachy-information . alist)
  (define docl-state-empty (list (list)))

  (define (call-with-docl get-source-identifier get-source-position input proc docl-state)
    "{any:input -> any} {input -> any} any {input -> any} list -> any/string
    installs a handler that amends exceptions with source information so that an exception-handler
    receives the arguments (key source-name source-position other-exception-arguments ...) for exceptions matching exception-keys.
    sets up the circular inclusion protection and bindings which are accessible with
    the procedures from (sph lang docl env). then calls \"proc\" with \"input\""
    (let (source-name (get-source-identifier input))
      (if (and source-name (contains? (first docl-state) source-name)) ""
        (let (docl-state (pair (pair source-name (first docl-state)) (tail docl-state)))
          (begin-first
            (catch #t (thunk (proc input docl-state))
              (l (key . args)
                (error-create key args
                  (pair source-name (and get-source-position (get-source-position input))))))
            (pair (tail (first docl-state)) (tail docl-state)))))))

  (define (docl-translate-any input proc docl-state)
    "calls proc with input and enables docl features as with call-with-docl.
    input can be anything, and the source-position argument for exceptions will be false."
    (call-with-docl equal-hash #f input proc docl-state))

  (define (docl-translate-port input proc docl-state)
    "port procedure:{port -> any} list -> any
    calls proc with input and enables docl features as with call-with-docl.
    input must be a port, and the source-identifier and source-position arguments for exceptions are set using port-filename and port-position"
    (call-with-docl port-filename port-position input proc docl-state))

  (define (docl-state-add-parameters docl-state a)
    "list false/([key value] ...) -> list
    add parameters from a flat list"
    (if a (pair (first docl-state) (append (list->alist a) (tail docl-state))) docl-state)))
