(library (sph log)
  (export
    log-default-formatter
    log-default-route
    log-message
    log-routes)
  (import
    (rnrs base)
    (sph)
    (sph list)
    (only (guile)
      display
      simple-format
      current-error-port
      current-time
      string-drop-right
      string-drop)
    (only (sph list) list-set-match-contains?)
    (only (sph string) any->string-write any->string string-replace-chars)
    (only (sph time) current-local-datetime-string))

  ;a diagnostic logging system with routing

  (define (apply-log-route arg categories args)
    (let (message ((vector-ref arg 1) categories args))
      ;this could easily be extended to dynamically open ports or for other ways to pass the message
      (each (l (port) (display message port)) (vector-ref arg 2))))

  (define (log-default-formatter categories args)
    "(symbol ...) (any ...) ->
    categories is the list of symbol names for which the log-route has matched"
    (simple-format #f "~A ~A\n  ~A\n"
      (current-local-datetime-string) categories
      (string-replace-chars (string-drop-right (string-drop (any->string args) 1) 1)
        (list (list #\newline #\newline #\space #\space)))))

  (define log-default-route (vector (q all) log-default-formatter (list (current-error-port))))

  (define (log-message categories . args)
    "symbol/(symbol ...) ->
    filters log-routes and calls any matching log-route formatter with args.
    categories can be a tree-like list with prefixed symbols some/every/none.
    log-route: #(symbol/(symbol ...) procedure:{list list ->} (port:output-port ...))"
    (let (categories (if (symbol? categories) (list categories) categories))
      (each
        (l (log-route)
          (if
            (or (eqv? (q all) (vector-ref log-route 0))
              (list-set-match-contains? categories (vector-ref log-route 0)))
            (apply-log-route log-route categories args)))
        log-routes)))

  (define log-routes (list log-default-route)))