(library (sph lang itml eval)
  (export
    itml-eval
    itml-eval*
    itml-eval-asc-indent-expr
    itml-eval-asc-inline-expr
    itml-eval-asc-line-expr
    itml-eval-call
    itml-eval-call-proc
    itml-eval-desc-indent-expr
    itml-eval-desc-indent-scm-expr
    itml-eval-desc-inline-scm-expr
    itml-eval-desc-line-expr
    itml-eval-desc-line-scm-expr
    itml-eval-descend
    itml-eval-descend-string
    itml-eval-file
    itml-eval-list
    itml-eval-port
    itml-eval-string
    itml-state-copy
    itml-state-create
    itml-state-data
    itml-state-depth
    sph-lang-itml-eval-description)
  (import
    (guile)
    (rnrs exceptions)
    (rnrs io ports)
    (sph)
    (sph hashtable)
    (sph lang itml read)
    (sph list)
    (sph tree))

  (define sph-lang-itml-eval-description
    "evaluate itml inline code expressions and possibly translate to a new format.
     itml inline code expressions can evaluate to non-itml syntax for the target language.
     # data structures
     itml-state: ((any:source-id ...) integer:depth hashtable:data)")

  (define (get-source-id input) "any -> any:identifier"
    (or (and (port? input) (port-filename input)) (ht-hash-equal input)))

  (define (itml-state-data a) (list-ref a 2))
  (define (itml-state-depth a) (list-ref a 1))

  (define* (itml-state-create depth env #:optional (data (ht-create-symbol)))
    "integer environment hashtable ... -> list
     data, if passed, will be modified"
    ; is a list because the first to values get updated
    (ht-set-q! data env env) (list (list) depth data))

  (define (itml-state-copy a) (list (first a) (second a) (ht-tree-copy (itml-state-data a))))

  (define (itml-eval-call input itml-state proc)
    "any procedure:{any:input list:itml-state} -> any
     protects against circular inclusion by checking and updating itml-state"
    ; would ideally add source-name and source-position to the exception but r6rs exceptions conditions are opaque objects
    ; and we could not find any documentation on accessors to be able to display their contents.
    (let (id (get-source-id input))
      (if (contains? (first itml-state) id) #f
        (let (itml-state (pair (pair id (first itml-state)) (tail itml-state)))
          (proc input itml-state)))))

  (define itml-eval
    (let
      ( (descend-proc
          (l (proc)
            (l (a re-descend sources depth data)
              "list procedure any integer any -> (result continue sources depth data)
              receive elements that are lists while mapping and eventually recursing sub-lists"
              (let (b (proc a (compose first re-descend) sources depth data))
                (if b (list b #f sources depth data) (list #f #t sources (+ 1 depth) data))))))
        (ascend-proc
          (l (proc)
            (l (a sources depth data)
              (list
                (proc a sources
                  ; depth 0 and 1 are equivalent
                  (max 0 (- depth 1)) data)
                sources (- depth 1) data))))
        (terminal-proc (l (proc) (l (a . b) (pair (apply proc a b) b)))))
      (l (a itml-state descend ascend terminal)
        "list:parsed-itml list procedure procedure procedure -> any"
        (map
          (l (a)
            (if (list? a)
              (first
                (apply tree-transform* a
                  (descend-proc descend) (ascend-proc ascend) (terminal-proc terminal) itml-state))
              (apply terminal a itml-state)))
          a))))

  (define itml-eval*
    (let
      ( (default-descend-alt (l a #f)) (default-ascend-alt (l (a . b) a))
        (dispatch
          (l (prefix-ht alt) "hashtable procedure list any ... -> false/any"
            (l (a . b)
              "list ascend/descend-arguments ... -> any
              selects a handler by list prefix and calls alt if no handler could be found.
              prefix-ht is only queried for lists with a symbol as first element"
              (let* ((prefix (first a)) (c (and (symbol? prefix) (ht-ref prefix-ht prefix))))
                (if c (apply c (tail a) b) (apply alt a b)))))))
      (l* (descend-prefix-ht ascend-prefix-ht #:optional terminal descend-alt ascend-alt)
        "hashtable  hashtable [procedure procedure procedure] -> procedure
        returns a procedure similar to itml-eval that uses expression handler procedures from hashtables.
        the -prefix-ht hashtables map list prefixes to tail handlers that map a expression to a result.
        if the prefix is not found in one of the hashtables then the corresponding -alt procedure is called"
        (l (a itml-state) "list list -> any"
          (itml-eval a itml-state
            (dispatch descend-prefix-ht (or descend-alt default-descend-alt))
            (dispatch ascend-prefix-ht (or ascend-alt (l (a . b) a)))
            (or terminal default-ascend-alt))))))

  (define (itml-eval-list a env state)
    "(symbol any ...) environment list -> any
     creates the syntax for a lambda that contains the code from the itml expression,
     and uses eval to create a procedure from it.
     the procedure is called with the itml state object from the call to evaluate the itml.
     this supports the use of syntax in itml expressions"
    ; debugging tip: log the literal passed to eval
    (let (proc (eval (qq (lambda (s) ((unquote (first a)) s (unquote-splicing (tail a))))) env))
      (proc state)))

  (define (itml-eval-descend a re-descend sources depth data) "evaluate an inline code expression"
    (itml-eval-list a (ht-ref-q data env) (list sources depth data)))

  (define (itml-eval-descend-string a . b)
    "list procedure integer list environment -> any
     evaluate an inline code expression when all elements are strings or string lists.
     converts the prefix to a symbol and prepares lists to evaluate to lists"
    (let
      (a
        (pair (string->symbol (first a))
          ;(tree-map-lists (l (a) (pair (q list) a)) (tail a))
          (tail a)))
      (apply itml-eval-descend a b)))

  (define (descend->ascend proc) (l (a . b) (apply proc a #f b)))
  (define itml-eval-desc-line-scm-expr itml-eval-descend)
  (define itml-eval-desc-inline-scm-expr itml-eval-descend)
  (define itml-eval-desc-indent-scm-expr itml-eval-descend)
  (define itml-eval-desc-line-expr itml-eval-descend-string)
  (define itml-eval-desc-indent-expr itml-eval-descend-string)
  (define itml-eval-asc-inline-expr (descend->ascend itml-eval-descend-string))
  (define itml-eval-asc-line-expr (descend->ascend itml-eval-descend-string))
  (define itml-eval-asc-indent-expr (descend->ascend itml-eval-descend-string))
  ;
  ;-- for reading from various sources
  ;
  (define (itml-eval-port itml-eval a . b) (apply itml-eval (port->itml-parsed a) b))
  (define (itml-eval-string itml-eval a . b) (apply itml-eval (string->itml-parsed a) b))

  (define (itml-eval-file itml-eval a . b)
    (apply itml-eval (call-with-input-file a (l (a) (port->itml-parsed a))) b))

  (define (itml-eval-call-proc itml-eval-any itml-eval)
    "symbol procedure -> procedure
     extend an itml-eval-* procedure to be passed directly to itml-call-for-*"
    (l (a . b) (apply itml-eval-any itml-eval a b))))
