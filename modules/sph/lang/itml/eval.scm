(library (sph lang itml eval)
  (export
    itml-call-for-eval
    itml-call-for-eval-any
    itml-call-for-eval-port
    itml-eval
    itml-eval-asc-indent-expr
    itml-eval-asc-inline-expr
    itml-eval-asc-line-expr
    itml-eval-desc-indent-expr
    itml-eval-desc-indent-scm-expr
    itml-eval-desc-inline-scm-expr
    itml-eval-desc-line-expr
    itml-eval-desc-line-scm-expr
    itml-eval-descend
    itml-eval-descend-string
    itml-eval-list
    itml-state-create
    itml-state-data
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

  (define (itml-state-data a) (first (tail (tail a))))
  (define (itml-state-create depth env) (list (list) depth (ht-create-symbol (q env) env)))

  (define (itml-call-for-eval input get-source-identifier get-source-position itml-state proc)
    "any procedure:{any -> any} procedure:{any -> any} list procedure:{any:input list:itml-state} -> any/string
     * protects against circular inclusion
     * adds source-name and source-position to exceptions
     exception-object: (obj source-name source-position)"
    (let (name (get-source-identifier input))
      (if (and name (contains? (first itml-state) name)) ""
        (let (itml-state (pair (pair name (first itml-state)) (tail itml-state)))
          (guard (obj (#t (raise (list (q itml) obj name (get-source-position input)))))
            (proc input itml-state))))))

  (define (itml-call-for-eval-port input itml-state proc)
    "port list procedure:{list:parsed-itml list -> any} list -> any
     input must be a port and the source-identifier and source-position for itml-call-for-eval are retrieved with port-filename and port-position.
     reads itml from port"
    (itml-call-for-eval input port-filename
      port-position itml-state (l (input itml-state) (proc (port->itml-parsed input) itml-state))))

  (define (itml-call-for-eval-any input itml-state proc)
    "port procedure:{port -> any} list -> any
     input can be of any type. source-identifier is created with a hash function and source-position will be false"
    (itml-call-for-eval input ht-hash-equal (const #f) itml-state proc))

  (define itml-eval
    (let
      ( (descend-proc
          (l (proc)
            (l (a re-descend sources depth data)
              (debug-log proc)
              (let (r (proc a re-descend sources depth data))
                (if r (list r #f sources depth data) (list #f #t sources (+ 1 depth) data))))))
        (ascend-proc
          (l (proc)
            (l (a sources depth data)
              (list
                (proc a sources
                  ; depth 0 and 1 are equivalent
                  (max 0 (- depth 1)) data)
                sources (- depth 1) data)))))
      (l (a itml-state descend ascend terminal)
        "list:parsed-itml list procedure procedure procedure -> any"
        (map
          (l (a)
            (if (list? a)
              (first
                (apply tree-transform-with-state a
                  (descend-proc descend) (ascend-proc ascend) terminal itml-state))
              (apply terminal a itml-state)))
          a))))

  (define (itml-eval-list a env state)
    (let (proc (eval (qq (l (b) ((unquote (first a)) b (unquote-splicing (tail a))))) env))
      (proc state)))

  (define (itml-eval-descend a re-descend sources depth data) "evaluate an inline code expression"
    (itml-eval-list a (ht-ref-q data env) (list sources depth data)))

  (define (itml-eval-descend-string a . b)
    "list procedure integer list environment -> any
     evaluate an inline code expression when the arguments are strings"
    (let (a (pair (string->symbol (first a)) (map (l (a) (list (q quote) a)) (tail a))))
      (apply itml-eval-list a b)))

  (define (descend->ascend proc) (l (a . b) (apply proc a #f b)))
  (define itml-eval-desc-line-scm-expr itml-eval-descend)
  (define itml-eval-desc-inline-scm-expr itml-eval-descend)
  (define itml-eval-desc-indent-scm-expr itml-eval-descend)
  (define itml-eval-desc-line-expr itml-eval-descend-string)
  (define itml-eval-desc-indent-expr itml-eval-descend-string)
  (define itml-eval-asc-inline-expr (descend->ascend itml-eval-descend-string))
  (define itml-eval-asc-line-expr (descend->ascend itml-eval-descend-string))
  (define itml-eval-asc-indent-expr (descend->ascend itml-eval-descend-string)))
