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
                  ((path-port)
                    (let (path (named-pipe))
                      (c path (nullary
                          (debug-log out "her")
                          (file->port path out)))))
                  ((port-path)
                    (c (nullary (open out O_WRONLY)) #f)) (else (c out #f)))))
            (map-output-input
              (l (current next c)
                "symbol symbol procedure:{current-output next-input} -> any
                map the current output type to the next input type and create the appropriate input output arguments.
                opening a named pipe blocks until the other end is open. while there may be alternative ways using O_RDWR (less portable) or O_NONBLOCK,
                we have not been able to make them work reliably. that is why procedures are passed"
                (case (join-symbols current next)
                  ((path-port) (let (path (named-pipe)) (c path (nullary (open path O_RDONLY)))))
                  ((port-path) (let (path (named-pipe)) (c (nullary (open path O_WRONLY)) path)))
                  ((port-port) (call-with-pipe (l a (apply c (reverse a)))))
                  ((path-path) (let (path (named-pipe)) (c path path))) (else (c #f #f))))))
          (l (first-input last-output . config)
            "string/port/any string/port/any #(symbol symbol procedure:{string/port string/port -> any}) ... -> (any ...)
            first-input last-output (path/port/nothing path/port/nothing procedure:{in out -> result}) ... -> (result ...)
            like pipe-chain but additionally supports specifying the type of port between procedures.
            procedures can take file paths or ports for input or output. input output combinations between procedures are automatically made compatible.
            caveats
            * if output and next output are path-port, then the input passed to the subsequent procedure is a procedure with no arguments that returns the port.
              it will block, therefore open it in another thread or process. the same applies for a port-path combination, where the preceding output will be a procedure
            * ports are not automatically closed, even when an exception occurs, and temporary files are stored in the systems temporary directory.
            * to make it possible to read while a previous procedure is writing a user might want to create threads or processes.
            * named pipes are used when paths are requested. they block for each end until the other end is connected, and they do not emit end of file until they are closed
            example:
            (path-pipe-chain \"/tmp/test\" (current-output-port) (vector (q nothing) (q port) (l (in out) (file->port in out))) (vector (q path) (q port) (l (in out) (do-stuff path port))))"
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
                                (begin-first
                                  (proc in out)
                                  (if transfer-out (begin-thread (transfer-out)))
                                  )
                                result)))
                          (map-output-input current next
                            (l (out next-in) (if transfer-in (begin-thread (transfer-in)))
                              (loop next-in (+ 2 types-index) rest (pair (proc in out) result)))))))))))))))))
