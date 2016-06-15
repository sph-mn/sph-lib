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
    (only (sph string)
      any->string-write
      any->string
      string-replace-chars)
    (only (sph time string) current-local-datetime-string))

  ;diagnostic logging with routing

  (define (apply-log-route a categories arguments)
    (let (message ((vector-ref a 1) categories arguments))
      ;this could easily be extended to dynamically open ports or for other ways to pass the message
      (each (l (port) (display message port)) (vector-ref a 2))))

  (define (log-default-formatter categories arguments)
    "(symbol ...) (any ...) ->
    categories is the list of symbol names for which the log-route has matched"
    (simple-format #f "~A ~A\n  ~A\n"
      (current-local-datetime-string) (simplify categories)
      (string-replace-chars (string-drop-right (string-drop (any->string arguments) 1) 1)
        (list (list #\newline #\newline #\space #\space)))))

  (define log-default-route (vector (q all) log-default-formatter (list (current-error-port))))
  (define log-routes (list log-default-route))

  (define (log-message categories . arguments)
    "symbol/(symbol ...) any ... ->
    filters log-routes and calls any matching log-route formatter with arguments.
    categories can be a tree-like list with prefixed symbols some/every/none.
    log-route: #(symbol/(symbol ...) procedure:{list list ->} (port:output-port ...))"
    (let (categories (if (symbol? categories) (list categories) categories))
      (each
        (l (log-route)
          (if
            (or (eqv? (q all) (vector-ref log-route 0))
              (list-set-match-contains? categories (vector-ref log-route 0)))
            (apply-log-route log-route categories arguments)))
        log-routes))))
