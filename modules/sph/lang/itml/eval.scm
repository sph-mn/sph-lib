(library (sph lang itml eval)
  (export
    itml-call-for-eval
    itml-call-for-eval-any
    itml-call-for-eval-port
    itml-eval
    itml-eval*
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

  (define (itml-state-create depth env . data)
    (list (list) depth (ht-from-list (pairs (q env) env data) eq? ht-hash-symbol)))

  (define (itml-call-for-eval input get-source-id get-source-name get-source-position itml-state proc)
    "any procedure:{any -> any} procedure:{any -> any} list procedure:{any:input list:itml-state} -> any/string
     * protects against circular inclusion
     * adds source-name and source-position to exceptions
     exception-object: (obj source-name source-position)"
    (let (name (get-source-id input))
      (if (and name (contains? (first itml-state) name)) ""
        (let (itml-state (pair (pair name (first itml-state)) (tail itml-state)))
          (guard (obj (#t (raise (list (q itml) obj (get-source-name input) (get-source-position input)))))
            (proc input itml-state))))))

  (define (itml-call-for-eval-port input itml-state proc)
    "port list procedure:{list:parsed-itml list -> any} -> any
     input must be a port and the source-identifier and source-position for itml-call-for-eval are retrieved with port-filename and port-position.
     reads itml from port"
    (itml-call-for-eval input port-filename port-filename port-position itml-state proc))

  (define (itml-call-for-eval-any input itml-state proc)
    "port procedure:{port -> any} list -> any
     input can be of any type. source-identifier is created with a hash function and source-position will be false"
    (itml-call-for-eval input ht-hash-equal identity (const #f) itml-state proc))

  (define itml-eval
    (let
      ( (descend-proc
          (l (proc)
            (l (a re-descend sources depth data)
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
              (let (c (ht-ref prefix-ht (first a))) (if c (apply c (tail a) b) (apply alt a b)))))))
      (l* (descend-prefix-ht ascend-prefix-ht #:optional terminal descend-alt ascend-alt)
        "hashtable  hashtable [procedure procedure procedure] -> procedure
        returns a procedure similar to itml-eval that uses expression handlers from hashtables.
        the -prefix-ht hashtables map list prefixes to tail handlers that map a list/expression to a result.
        if the prefix is not found in one of the hashtables then the corresponding -alt procedure is called"
        (l (a itml-state) "list list -> sxml"
          (itml-eval a itml-state
            (dispatch descend-prefix-ht (or descend-alt default-descend-alt))
            (dispatch ascend-prefix-ht (or ascend-alt (l (a . b) a)))
            (or terminal default-ascend-alt))))))

  (define (itml-eval-list a env state)
    (let (proc (eval (qq (l (b) ((unquote (first a)) b (unquote-splicing (tail a))))) env))
      (proc state)))

  (define (itml-eval-descend a re-descend sources depth data) "evaluate an inline code expression"
    (itml-eval-list a (ht-ref-q data env) (list sources depth data)))

  (define (itml-eval-descend-string a . b)
    "list procedure integer list environment -> any
     evaluate an inline code expression when the arguments are strings"
    (let (a (pair (string->symbol (first a)) (tail a))) (apply itml-eval-descend a b)))

  (define (descend->ascend proc) (l (a . b) (apply proc a #f b)))
  (define itml-eval-desc-line-scm-expr itml-eval-descend)
  (define itml-eval-desc-inline-scm-expr itml-eval-descend)
  (define itml-eval-desc-indent-scm-expr itml-eval-descend)
  (define itml-eval-desc-line-expr itml-eval-descend-string)
  (define itml-eval-desc-indent-expr itml-eval-descend-string)
  (define itml-eval-asc-inline-expr (descend->ascend itml-eval-descend-string))
  (define itml-eval-asc-line-expr (descend->ascend itml-eval-descend-string))
  (define itml-eval-asc-indent-expr (descend->ascend itml-eval-descend-string)))
