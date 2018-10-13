(library (sph lang itml eval)
  (export
    itml-eval
    itml-eval*
    itml-eval-asc-indent-expr
    itml-eval-asc-inline-expr
    itml-eval-asc-line-expr
    itml-eval-call
    itml-eval-call-f
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
    itml-state-data-bindings
    itml-state-data-exceptions
    itml-state-data-recursion
    itml-state-data-unsafe-eval
    itml-state-data-user
    itml-state-depth
    itml-state-stack
    sph-lang-itml-eval-description)
  (import
    (guile)
    (ice-9 sandbox)
    (rnrs exceptions)
    (rnrs io ports)
    (sph)
    (sph hashtable)
    (sph lang itml read)
    (sph list)
    (sph string)
    (sph tree)
    (sph vector))

  (define sph-lang-itml-eval-description
    "evaluate itml inline code expressions and possibly translate to a new format.
     itml inline code expressions can evaluate to non-itml syntax for the target language.
     # data structures
     itml-state: ((any:source-id ...) integer:depth hashtable:data)")

  (define (get-source-id input) "any -> any:identifier"
    (or (and (port? input) (port-filename input)) (ht-hash-equal input)))

  (define (itml-state-stack a) (list-ref a 0))
  (define (itml-state-depth a) (list-ref a 1))
  (define (itml-state-module a) (list-ref a 2))
  (define (itml-state-data a) (list-ref a 3))
  (define itml-state-data-bindings (vector-accessor 0))
  (define itml-state-data-exceptions (vector-accessor 1))
  (define itml-state-data-recursion (vector-accessor 2))
  (define itml-state-data-unsafe-eval (vector-accessor 3))
  (define itml-state-data-user (vector-accessor 4))

  (define*
    (itml-state-create #:key (bindings all-pure-bindings) module depth exceptions recursion
      unsafe-eval
      user-data)
    "integer sandbox-module-bindings boolean any ... -> list
     #:bindings
       must be a bindings list for make-sandbox-module from (ice-9 sandbox)
       should be unset or false when using #:unsafe-eval
       #:module has preference
     #:exceptions
       if true, all exceptions from evaluated expressions are catched and a placeholder string is returned.
       default is false
     #:recursion
       if true, all itml environment bindings called receive a copy of
       the itml-state as the first argument and circular inclusion is prevented
       when using itml-eval with the state
     #:user-data
       custom user data variable of any type
       should be serialisable with scheme write when unsafe-eval is false and recursion is true
     #:unsafe-eval
       use eval instead of eval-in-sandbox to evaluate expressions and if recursion is true, pass a state object that can contain
       references to the dynamic environment and is not serialised.
       instead of using #:bindings a module should be passed via #:module. the module can be created with (rnrs eval) \"environment\".
       if only bindings is set, a module is created with make-sandbox-module, which has not been tested to work with unsafe-eval
     #:depth
       current call nesting depth. in case itml expression evaluators need it"
    ; datatype list because the first two values get updated
    (list (list) (or depth 0)
      (or module (make-sandbox-module bindings))
      (vector (and (not module) bindings) exceptions recursion unsafe-eval user-data)))

  (define (itml-state-copy a) "user-data is not deep-copied"
    (list (itml-state-stack a) (itml-state-depth a)
      (itml-state-module a) (vector-copy (itml-state-data a))))

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
      ( (descend-f
          (l (proc)
            (l (a re-descend stack depth module data)
              "list procedure any integer any -> (result continue stack depth data)
              receive elements that are lists while mapping and eventually recursing sub-lists"
              (let (b (proc a (compose first re-descend) stack depth module data))
                (if b (list b #f stack depth module data)
                  (list #f #t stack (+ 1 depth) module data))))))
        (ascend-f
          (l (proc)
            (l (a stack depth module data)
              (list
                (proc a stack
                  ; depth 0 and 1 are equivalent
                  (max 0 (- depth 1)) module data)
                stack (- depth 1) module data))))
        (terminal-f (l (proc) (l (a . b) (pair (apply proc a b) b)))))
      (l (a itml-state descend ascend terminal)
        "list:parsed-itml list procedure procedure procedure -> any"
        (map
          (l (a)
            (if (list? a)
              (first
                (apply tree-transform* a
                  (descend-f descend) (ascend-f ascend) (terminal-f terminal) itml-state))
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
      (l* (descend-prefix-ht ascend-prefix-ht #:key terminal descend-alt ascend-alt)
        "hashtable  hashtable [procedure procedure procedure] -> procedure
        returns a procedure similar to itml-eval that uses expression handler procedures from hashtables.
        the -prefix-ht hashtables map list prefixes to tail handlers that map a expression to a result.
        if the prefix is not found in one of the hashtables then the corresponding -alt procedure is called"
        (l (a itml-state) "list list -> any"
          (itml-eval a itml-state
            (dispatch descend-prefix-ht (or descend-alt default-descend-alt))
            (dispatch ascend-prefix-ht (or ascend-alt (l (a . b) a)))
            (or terminal default-ascend-alt))))))

  (define (itml-eval-list a stack depth module data)
    "(symbol any ...) sandbox-module boolean list -> any
     creates the syntax for a lambda that contains the code from the itml expression,
     and evaluates it using eval-in-sandbox.
     the procedure is called with the itml state, which is also available in arguments in a variable named \"s\".
     the use of syntax in itml expressions is supported.
     itml-state is passed serialised with the field for the sandbox-module set to false. for nested itml-eval-list calls, for example
     when other itml files can be included, the sandbox-module can be recreated from the make-sandbox-module bindings
     in itml-state-data"
    ; debugging tip: log expression literal passed to eval
    (let
      (thunk
        (if (itml-state-data-unsafe-eval data)
          (nullary "pass the the full itml-state"
            ( (eval (qq (lambda (s) ((unquote (first a)) s (unquote-splicing (tail a))))) module)
              (list stack depth module data)))
          (nullary "can only pass a reduced, serialised itml-state"
            (eval-in-sandbox
              (if (itml-state-data-recursion data)
                (qq
                  ( (lambda (s) ((unquote (first a)) s (unquote-splicing (tail a))))
                    (quote (unquote (list stack depth #f data)))))
                a)
              #:time-limit 120 #:allocation-limit 1000000000 #:module module #:sever-module? #f))))
      (if (itml-state-data-exceptions data) (thunk) (false-if-exception (thunk)))))

  (define (itml-eval-descend a re-descend stack depth module data)
    "evaluate an inline code expression" (itml-eval-list a stack depth module data))

  (define (itml-eval-descend-string a . b)
    "list procedure integer list module -> any
     evaluate an inline code expression when all elements are strings or string lists.
     converts the prefix to a symbol and prepares lists to evaluate to lists"
    (let (a (pair (string->symbol (first a)) (tail a))) (apply itml-eval-descend a b)))

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

  (define (itml-eval-call-f itml-eval-any itml-eval)
    "symbol procedure -> procedure
     extend an itml-eval-* procedure to be passed directly to itml-call-for-*"
    (l (a . b) (apply itml-eval-any itml-eval a b))))
