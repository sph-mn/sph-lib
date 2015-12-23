(library (sph lang itml)
  (export
    itml-ascend-proc
    itml-descend-proc
    itml-parsed->result-proc)
  (import
    (rnrs base)
    (sph)
    (only (sph tree) tree-transform-with-state))

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
      (debug-log a)
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
        nesting-depth states))))
