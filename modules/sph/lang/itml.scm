(library (sph lang itml)
  (export
    itml-ascend-proc
    itml-descend-proc
    itml-parsed->result-proc)
  (import
    (sph)
    (only (sph tree) tree-transform-with-state))

  (define sph-lang-itml-description "helpers for converting from itml to other formats")

  (define (itml-ascend-proc create-result adjust-depth)
    "procedure:{any integer any ... -> any} procedure:{integer -> integer} -> procedure:{any:expression integer any ... -> any}
     creates a ascend procedure for tree-transform-with-state that tracks nesting depth"
    (l (a depth . states) "list integer any ...  -> any"
      (pairs (apply create-result a (adjust-depth depth) states) (- depth 1) states)))

  (define (itml-descend-proc create-result)
    "procedure:{any recurse integer any ... -> any} -> procedure
     creates a descend procedure for tree-transform-with-state that tracks nesting depth
     and does not support automatic re-parsing of result expression"
    (l (a re-descend depth . states)
      (let (r (apply create-result a re-descend depth states))
        (if r (pairs r #f depth states) (pairs #f #t (+ 1 depth) states)))))

  (define
    (itml-parsed->result-proc create-result descend ascend handle-top-level-terminal
      handle-terminal)
    "procedure procedure procedure procedure -> procedure:{any:expression integer:depth any ... -> any}"
    (l (a depth . states)
      "list integer any ... -> any
      a translator for parsed-itml. pass nesting depth and custom states to descend, ascend and terminal procedures.
      uses (sph tree) tree-transform-with-state "
      (apply create-result
        (map
          (l (a)
            (if (list? a)
              (first
                (apply tree-transform-with-state a descend ascend handle-terminal depth states))
              (apply handle-top-level-terminal a depth states)))
          a)
        depth states))))
