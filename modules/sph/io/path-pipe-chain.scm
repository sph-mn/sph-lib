(library (sph io path-pipe-chain)
  (export
    path-pipe-chain)
  (import
    (guile)
    (ice-9 threads)
    (sph)
    (sph io)
    (sph one)
    (sph vector))

  (define path-pipe-chain
    (let-syntax
      ( (join-symbols
          (syntax-rule (a b)
            (string->symbol
              (string-append (if a (symbol->string a) "nothing") "-"
                (if b (symbol->string b) "nothing"))))))
      (let (any->type (l (a) (if (string? a) (q path) (if (port? a) (q port) #f))))
        (let
          ( (get-output-types
              (l (first-input last-output config)
                (list->vector
                  (append
                    (fold-right (l (a r) (pairs (vector-first a) (vector-second a) r)) (list)
                      config)
                    (list (any->type last-output))))))
            (get-procedures (l (config) (map (l (a) (vector-ref a 2)) config)))
            (map-first-input
              (l (previous current in c)
                "symbol symbol any procedure:{current-input procedure:transfer} -> any"
                (case (join-symbols previous current) ((path-port) (c (open-file previous "r") #f))
                  ( (port-path)
                    (let (path (named-pipe))
                      (c path
                        (nullary
                          (let (file (open path (logior O_WRONLY O_NONBLOCK)))
                            (port-copy-all in file) (close file))))))
                  (else (c in #f)))))
            (map-last-output
              (l (current next out c)
                "symbol symbol any:last-output procedure:{output procedure:transfer} -> any
              map the last procedures output to the chains last output.
              inbetween arguments are created in the required types but the chains last output is of a fixed type and so a nullary transfer
              procedure might be passed for transferring data from the last procedures output to the given
              last output"
                (case (join-symbols current next)
                  ( (path-port)
                    (let (path (named-pipe))
                      (c path
                        (nullary
                          (let (file (open path (logior O_RDONLY O_NONBLOCK)))
                            (port-copy-all file out) (close file))))))
                  ((port-path) (c (open out O_WRONLY) #f)) (else (c out #f)))))
            (map-output-input
              (l (current next c)
                "symbol symbol procedure:{current-output next-input} -> any
              map the current output type to the next input type and create the appropriate input output arguments"
                (case (join-symbols current next)
                  ( (path-port)
                    (let (path (named-pipe)) (c path (open path (logior O_RDONLY O_NONBLOCK)))))
                  ( (port-path)
                    (let (path (named-pipe)) (c (open path (logior O_RDWR O_NONBLOCK)) path)))
                  ((port-port) (call-with-pipe c))
                  ((path-path) (let (path (named-pipe)) (c path path))) (else (c #f #f))))))
          (l (first-input last-output . config)
            (if (null? config) config
              (let (types (get-output-types first-input last-output config))
                (map-first-input (any->type first-input) (vector-ref types 0)
                  first-input
                  (l (in transfer-in)
                    (let loop
                      ((in in) (types-index 0) (rest (get-procedures config)) (result (list)))
                      (let
                        ( (current (vector-ref types (+ 1 types-index)))
                          (next (vector-ref types (+ 2 types-index))) (proc (first rest))
                          (rest (tail rest)))
                        (if (null? rest)
                          (map-last-output current next
                            last-output
                            (l (out transfer-out)
                              (pair
                                (begin-first (proc in out)
                                  (if transfer-out (begin-thread (transfer-out))))
                                result)))
                          (map-output-input current next
                            (l (out next-in) (if transfer-in (begin-thread (transfer-in)))
                              (loop next-in (+ 2 types-index) rest (pair (proc in out) result)))))))))))))))))
