(library (sph io path-pipe-chain)
  (export
    path-pipe-chain
    sph-io-path-pipe-chain-description)
  (import
    (guile)
    (ice-9 threads)
    (sph)
    (sph io)
    (sph one)
    (sph vector))

  (define sph-io-path-pipe-chain-description
    "call procedures with input output arguments set up in a chained manner to allow data flow between them.
    procedures can request paths or ports as arguments and the links are made compatible automatically")

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
                (case (join-symbols previous current) ((path-port) (c (open in O_RDONLY) #f))
                  ((port-path) (let (path (named-pipe)) (c path (nullary (port->file in path)))))
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
                      (c path (nullary (call-with-input-file path (l (in) (port-copy-all in out)))))))
                  ((port-path) (c (open out (logior O_WRONLY O_CREAT)) #f)) (else (c out #f)))))
            (map-output-input
              (l (current next c)
                "symbol symbol procedure:{current-output next-input} -> any
                map the current output type to the next input type and create the appropriate input output arguments.
                just opening a named pipe normally blocks until the other end is open.
                this is problematic if it is to be set as standard input for the next process because it would require the open to be made in an extra new thread.
                the O_NONBLOCK allows the read end to be opened first. this does not work for opening the write end first, as does O_RDWR"
                (case (join-symbols current next)
                  ( (path-port)
                    (let (path (named-pipe)) (c path (open path (logior O_RDONLY O_NONBLOCK)))))
                  ((port-path) (let (path (named-pipe)) (c (nullary (open path O_WRONLY)) path)))
                  ((port-port) (let (pipe-ends (pipe)) (c (tail pipe-ends) (first pipe-ends))))
                  ((path-path) (let (path (named-pipe)) (c path path))) (else (c #f #f))))))
          (l (first-input last-output . config)
            "string/port/any string/port/any #(symbol symbol procedure:{string/port string/port -> any}) ... -> (any ...)
            first-input last-output (path/port/nothing path/port/nothing procedure:{in out -> result}) ... -> (result ...)
            like pipe-chain but additionally supports specifying the type of port between procedures.
            procedures can take file paths or ports for input or output. input output combinations between procedures are automatically made compatible.
            caveats:
            * in port-path links, output is a nullary procedure that is to be called in a separate thread to return the port.
              it blocks unless the next procedure has opened the other end.
            * intermediate ports must be closed after read/write finished
            * intermediate paths should be deleted after read/write finished. otherwise they accumulate in the systems temporary directory
            * ports are not automatically closed when an exception occurs
            * to make it possible to read while a previous procedure is writing a user might want to create threads or processes.
            * named pipes are used when paths are requested. they block for each end until the other end is connected, and they do not emit end of file until they are closed
            example:
            (path-pipe-chain \"/tmp/test\" (current-output-port) (vector (q nothing) (q port) (l (in out) (file->port in out))) (vector (q path) (q port) (l (in out) (do-stuff path port))))"
            (if (null? config) config
              (let (types (get-output-types first-input last-output config))
                (map-first-input (any->type first-input) (vector-ref types 0)
                  first-input
                  (l (in transfer-in)
                    (let loop ((in in) (types-index 0) (rest (get-procedures config)))
                      (let
                        ( (current (vector-ref types (+ 1 types-index)))
                          (next (vector-ref types (+ 2 types-index))) (proc (first rest))
                          (rest (tail rest)))
                        (if (null? rest)
                          (list
                            (map-last-output current next
                              last-output
                              (l (out transfer-out)
                                (let
                                  ( (transfer-in-thread
                                      (if transfer-in (begin-thread (transfer-in)) #f))
                                    (transfer-out-thread
                                      (if transfer-out (begin-thread (transfer-out)) #f)))
                                  (begin-first (proc in out)
                                    (if transfer-in-thread (join-thread transfer-in-thread))
                                    (if transfer-out-thread (join-thread transfer-out-thread)))))))
                          (map-output-input current next
                            (l (out next-in) (if transfer-in (begin-thread (transfer-in)))
                              (pair (proc in out) (loop next-in (+ 2 types-index) rest)))))))))))))))))
