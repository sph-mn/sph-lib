(library (sph lang itml)
  (export
    itml-ascend-proc
    itml-descend-proc
    itml-parsed->itml
    itml-parsed->result-proc)
  (import
    (guile)
    (ice-9 threads)
    (rnrs base)
    (sph)
    (sph hashtable)
    (sph lang indent-syntax)
    (sph lang parser itml)
    (sph string)
    (only (sph tree) flatten tree-transform-with-state))

  (define (itml-ascend-proc create-result adjust-nesting-depth) "environment integer -> procedure"
    (l (a nesting-depth . states) "list integer vhash environment -> any"
      (pairs (apply create-result a (adjust-nesting-depth nesting-depth) states)
        (- nesting-depth 1) states)))

  (define (itml-descend-proc create-result)
    (l (a re-descend nesting-depth . states)
      (let (r (apply create-result a re-descend nesting-depth states))
        (if r (pairs r #f nesting-depth states) (pairs #f #t (+ 1 nesting-depth) states)))))

  (define
    (itml-parsed->result-proc create-result descend ascend handle-top-level-terminal
      handle-terminal)
    (l (a nesting-depth . states)
      "list [integer vhash environment] -> any
      a translator for itml-parsed. pass nesting depth and custom states to descend, ascend and terminal procedures"
      (apply create-result
        (par-map
          (l (e)
            (if (list? e)
              (first
                (apply tree-transform-with-state e
                  descend ascend handle-terminal nesting-depth states))
              (apply handle-top-level-terminal e nesting-depth states)))
          a)
        nesting-depth states)))

  ; itml-parsed->itml
  (define (datum->string-write a) (call-with-output-string (l (port) (write a port))))
  (define (datum->string-display a) (call-with-output-string (l (port) (display a port))))
  (define (ascend-handle-line a nesting-depth) (apply string-append a))
  (define (descend-handle-double-backslash a nesting-depth) "\\\\")
  (define (ascend-handle-association a nesting-depth) (apply string-append (first a) ": " (tail a)))

  (define (handle-descend-line-scm-expr a nesting-depth)
    (apply string-append "\\." (first a) ": " (tail a)))

  (define (handle-descend-inline-scm-expr a nesting-depth) (apply string-append "\\." a))

  (define (handle-descend-indent-scm-expr a nesting-depth)
    (pair (string-append "\\." (first a)) (tail a)))

  (define (handle-descend-indent-expr a nesting-depth)
    (pair (string-append "\\#" (first a)) (tail a)))

  (define (handle-ascend-inline-expr a nesting-depth)
    (string-append "\\" (datum->string-display a)))

  (define (handle-ascend-line-expr a nesting-depth)
    (apply string-append "\\" (first a) ": " (tail a)))

  (define (handle-ascend-indent-expr a nesting-depth)
    (pair (string-append "\\" (first a)) (tail a)))

  (define (handle-escaped-association-infix a nesting-depth) (apply string-append "\\" a))

  (define-as ascend-prefix->handler-ht symbol-hashtable
    line ascend-handle-line
    inline-expr handle-ascend-inline-expr
    line-expr handle-ascend-line-expr
    indent-expr handle-ascend-indent-expr association ascend-handle-association)

  (define-as descend-prefix->handler-ht symbol-hashtable
    inline-scm-expr handle-descend-inline-scm-expr
    line-scm-expr handle-descend-line-scm-expr
    indent-scm-expr handle-descend-indent-scm-expr
    indent-descend-expr handle-descend-indent-expr
    double-backslash descend-handle-double-backslash
    escaped-association-infix handle-escaped-association-infix)

  (define-syntax-rule (expr->itml prefix->handler a proc-arguments ...)
    (let (p (hashtable-ref prefix->handler (first a))) (and p (p (tail a) proc-arguments ...))))

  (define (ascend-expr->itml a nesting-depth)
    (or (expr->itml ascend-prefix->handler-ht a nesting-depth) a))

  (define (descend-expr->itml a re-descend nesting-depth)
    (expr->itml descend-prefix->handler-ht a nesting-depth))

  (define (handle-top-level-terminal a . states) (if (eqv? (q line-empty) a) "" a))
  (define (handle-terminal a . states) (pair (handle-top-level-terminal a) states))

  (define itml-parsed->itml
    (itml-parsed->result-proc
      (l (a nesting-depth) (prefix-tree->indent-tree-string a nesting-depth))
      (itml-descend-proc descend-expr->itml) (itml-ascend-proc ascend-expr->itml identity)
      handle-top-level-terminal handle-terminal)))
