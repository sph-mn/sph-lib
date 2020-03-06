(define-module (sph log))
(use-modules (sph) (sph list) (sph time) (sph time string) (sph vector) (sph list) (sph string))

(export log-default-formatter log-default-route
  log-message log-routes-set! log-routes sph-log-description)

(define sph-log-description
  "diagnostic logging with routing by category to none or many configurable output-targets")

(define (apply-log-route a categories arguments)
  (let (message ((vector-ref a 1) categories arguments))
    (each (l (port) (display message port)) (vector-ref a 2))))

(define (log-default-formatter categories arguments)
  "(symbol ...) (any ...) ->
   categories is the list of symbol names for which the log-route has matched"
  (simple-format #f "~A ~A\n  ~A\n"
    (utc-current-ymd-ks) (simplify categories)
    (string-replace-chars (string-drop-right (string-drop (any->string arguments) 1) 1)
      (list (list #\newline #\newline #\space #\space)))))

(define log-default-route (vector (q all) log-default-formatter (list (current-error-port))))
(define log-routes (list log-default-route))
(define (log-routes-get) log-routes)
(define (log-routes-set! a) (set! log-routes a))

(define (log-message categories . arguments)
  "symbol/(symbol ...) any ... ->
   filters log-routes and calls any matching log-route formatter with arguments.
   categories can be a tree-like list with prefixed symbols and/or/not.
   log-route: #(symbol/(symbol ...) procedure:{list list ->} (port:output-port ...))"
  (let (categories (if (symbol? categories) (list categories) categories))
    (each
      (l (log-route)
        (if
          (or (eq? (q all) (vector-first log-route))
            (list-logical-contains? categories (vector-first log-route)))
          (apply-log-route log-route categories arguments)))
      log-routes)))
