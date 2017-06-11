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
      (let (any->type (l (a) (if (string? a) (q path) (if (port? a) (q port) (q nothing)))))
        (let
          ( (get-types
              (l (first-input config) "in reverse order \"ioioioo\""
                (list->vector
                  (fold (l (a r) (pairs (vector-second a) (vector-first a) r))
                    (list (any->type first-input)) config))))
            (get-procedures (l (config) (fold (l (a r) (pair (vector-ref a 2) r)) (list) config)))
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
            (map-input-output
              (l (current next c)
                "symbol symbol procedure:{current-output next-input} -> any
                map the current output type to the next input type and create the appropriate input output arguments.
                just opening a named pipe normally blocks until the other end is open.
                this is problematic if it is to be set as standard input for the next process because it would require the open to be made in an extra new thread.
                the O_NONBLOCK allows the read end to be opened first. this does not work for opening the write end first, as does O_RDWR"
                (case (join-symbols current next)
                  ((path-port) (let (path (named-pipe)) (c path (nullary (open path O_WRONLY)))))
                  ( (port-path)
                    (let (path (named-pipe)) (c (open path (logior O_RDONLY O_NONBLOCK)) path)))
                  ((port-port) (let (pipe-ends (pipe)) (c (first pipe-ends) (tail pipe-ends))))
                  ((path-path) (let (path (named-pipe)) (c path path))) (else (c #f #f))))))
          (l (first-input last-output . config)
            "string/port/any string/port/any #(symbol symbol procedure:{string/port string/port -> any}) ... -> (any ...)
            first-input last-output (path/port/nothing path/port/nothing procedure:{in out -> result}) ... -> (result ...)
            like pipe-chain but additionally supports specifying the type of port between procedures.
            procedures can take file paths or ports for input or output. input output combinations between procedures are automatically made compatible.
            caveats:
            * intermediate ports must be closed after read/write finished
            * intermediate paths should be deleted after read/write finished. otherwise they accumulate in the systems temporary directory
            * ports are not automatically closed when an exception occurs
            * to make it possible to read while a previous procedure is writing a user might want to create threads or processes
            * named pipes are used when paths are requested. they normally block for each end until the other end is connected, and they do not send end of file before the writer is closed
            example:
            (path-pipe-chain \"/tmp/test\" (current-output-port) (vector (q nothing) (q port) (l (in out) (file->port in out))) (vector (q path) (q port) (l (in out) (do-stuff path port))))"
            ; algorithm:
            ; * create a vector of all output/input type names in reverse order to get previous, current and next type per proc
            ; * create previous-current and current-next input/output link objects
            ; * handle first and last input/output separately because they are foreign objects not to be created by path-pipe-chain
            ; * prepare and call procedures in reverse order to complete the chain at the first input.
            ;   this means the input is passed to the current and the output to the next procedure.
            ;   this is because of named-pipes, whose readers can easily be opened before the writer but opening writers before readers blocks (even with O_RDWR).
            ;   in port-path links it would lead to passing a procedure that opens the writer to the next procedure,
            ;   to be called in a separate thread or process so it can block until the reader, which just receives a path, is open. this would be
            ;   (a) an inconsistency of parameter types and (b) it is not easy to do this in a new process in an ansync signal safe with guile
            (if (null? config) config
              (let (types (get-types first-input config))
                (map-last-output (any->type last-output) (vector-ref types 0)
                  last-output
                  (l (out transfer-out)
                    (let loop ((out out) (types-index 0) (rest (get-procedures config)))
                      (let
                        ( (current (vector-ref types (+ 1 types-index)))
                          (next (vector-ref types (+ 2 types-index))) (proc (first rest))
                          (rest (tail rest)))
                        (if (null? rest)
                          (list
                            (map-first-input current next
                              first-input
                              (l (in transfer-in)
                                (let
                                  ( (transfer-in-thread
                                      (if transfer-in (begin-thread (transfer-in)) #f))
                                    (transfer-out-thread
                                      (if transfer-out (begin-thread (transfer-out)) #f)))
                                  (begin-first (proc in (if (procedure? out) (out) out))
                                    (if transfer-in-thread (join-thread transfer-in-thread))
                                    (if transfer-out-thread (join-thread transfer-out-thread)))))))
                          (map-input-output current next
                            (l (in next-out) (if transfer-out (begin-thread (transfer-out)))
                              (pair (proc in out) (loop next-out (+ 2 types-index) rest)))))))))))))))))
