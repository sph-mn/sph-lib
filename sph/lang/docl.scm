(library (sph lang docl)
  (export
    call-with-docl
    docl-default-env-module-names
    docl-include-stack
    docl-state-empty
    docl-translate-any
    docl-translate-port)
  (import
    (ice-9 vlist)
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph error)
    (only (guile)
      current-thread
      port-filename
      catch)
    (only (rnrs hashtables) equal-hash)
    (only (sph list) contains?)
    (only (sph one) first-as-result)
    (only (sph two) vhash-refq vhash-setq))

  (define docl-default-env-module-names (q ((sph lang docl env default))))
  (define docl-state-empty vlist-null)

  (define (call-with-docl get-source-identifier get-source-position input proc docl-state)
    "{any:input -> any} {input -> any} symbol-hashtable/boolean boolean boolean/(symbol ..) any:input {input -> any} -> any/string
    installs a handler that amends exceptions with source information so that an exception-handler
    receives the arguments (key source-name source-position other-exception-arguments ...) for exceptions matching exception-keys.
    sets up the circular inclusion protection and bindings which are accessible with
    the procedures from (sph lang docl env). then calls \"proc\" with \"input\""
    (let (thread (current-thread))
      (let
        ( (source-name (get-source-identifier input))
          (recursion-stack (or (vhash-refq docl-state thread) (list))))
        (if (and source-name (contains? recursion-stack source-name)) ""
          (let*
            ( (recursion-stack (pair source-name recursion-stack))
              (docl-state (vhash-setq docl-state thread recursion-stack)))
            (first-as-result
              (catch #t (thunk (proc input docl-state))
                (l (key . args)
                  (error-create key args
                    (pair source-name (and get-source-position (get-source-position input))))))
              (vhash-setq docl-state thread (tail recursion-stack))))))))

  (define (docl-translate-any input proc docl-state)
    "calls proc with input and enables docl features as with call-with-docl.
    input can be anything, and the source-position argument for exceptions will be false."
    (call-with-docl equal-hash #f input proc docl-state))

  (define (docl-translate-port input proc docl-state)
    "port procedure:{port -> any} symbol-hashtable/boolean boolean -> any
    calls proc with input and enables docl features as with call-with-docl.
    input must be a port, and the source-identifier and source-position arguments for exceptions are set using port-filename and port-position"
    (call-with-docl port-filename port-position input proc docl-state)))
