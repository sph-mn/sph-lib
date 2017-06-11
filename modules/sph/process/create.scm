(library (sph process create)
  (export
    process-chain
    process-chain->string
    process-chain-finish
    process-chain-finish-success?
    process-chain-path-pipe
    process-create
    sph-process-create-description)
  (import
    (guile)
    (sph)
    (sph io)
    (sph io path-pipe-chain)
    (sph process)
    (only (sph list) any->list first-or-false)
    (only (sph one) search-env-path begin-first))

  (define sph-process-create-description "create child processes and process chains")
  (load-extension "libguile-sph-lib" "init_sph_lib")

  (define*
    (process-create executable #:optional (arguments (list)) input-port output-port error-port
      #:key
      env
      (keep-descriptors (list))
      (search-path? #t))
    "string:path/file-name [(string ...) port/integer/false ... #:key (env (environ)) (keep-descriptors (list)) search-path?] -> process-id
    \"executable\" is the path or file name of a file to execute to become the new process.
    if the given string for \"executable\" does not start with a slash and search-path? is true (default is false), it is searched in the directories in the PATH environment variable.
    the optional parameters are to set the standard streams.
    with the key parameters the environment variables for the new process can be set in the format (environ) returns.
    no file descriptors from the parent process are transferred to the child except if listed in keep-descriptors or given using the input/output/error-port parameters.
    returns the process id of the newly created child process or false if the process could not be created"
    (if search-path?
      (let
        (executable
          (if (string-prefix? "/" executable) executable
            (first-or-false (search-env-path executable))))
        (and executable (file-exists? executable)
          (primitive-process-create executable (pair (basename executable) arguments)
            input-port output-port error-port env keep-descriptors)))
      (and (file-exists? executable)
        (primitive-process-create executable (pair (basename executable) arguments)
          input-port output-port error-port env keep-descriptors))))

  ; the following definitions are not directly dependent on the loaded extension
  ;

  (define* (process-chain first-input last-output execute-arguments #:key (search-path? #t))
    "port/any port/any (string/(string:executable string:argument ...) ...) -> (integer:pid ...)
    creates a new process for each execute-argument and sets standard input and output of the processes in a chaining manner:
    input is the first-input or the input from the previous process, the output is the output to the next process or the last-output.
    error port for each process is the current-error-port of the process calling process-chain pipes.
    if any process could not be created then all previously created processed are sent SIGTERM and the result is an empty list.
    tip: if you want to call procedures in between you could start guile processes or use pipe-chain, create the processes yourself and threads for guile code"
    (let*
      ( (execute-arguments-length (length execute-arguments)) (error (current-error-port))
        (pids
          (apply pipe-chain first-input
            last-output
            (map
              (l (a)
                (let (executable-and-arguments (any->list a))
                  (l (in out)
                    (begin-first
                      (process-create (first executable-and-arguments)
                        (tail executable-and-arguments) in out error #:search-path? search-path?)
                      (if (not (eq? first-input in)) (close in))
                      (if (not (eq? last-output out)) (close out))))))
              execute-arguments))))
      (if (= (length pids) execute-arguments-length) pids
        (begin (each (l (a) (kill a SIGTERM)) pids) (list)))))

  (define (process-chain-finish process-chain-result)
    "false/(integer:pid ...) -> integer
    wait for the termination of the processes and return the exit status"
    (and process-chain-result (every process-finish process-chain-result)))

  (define (process-chain-finish-success? process-chain-result)
    "false/(integer:pid ...) -> boolean
    wait for the termination of the processes and check if its exit status is 0"
    (and process-chain-result (every process-finish-success? process-chain-result)))

  (define (process-chain->string first-input . process-chain-arguments)
    "false/port any ... -> string"
    (call-with-pipe
      (l (in out)
        (let (pids (apply process-chain first-input out process-chain-arguments)) (close out)
          (begin-first (port->string in) (process-chain-finish pids))))))

  (define* (process-chain-path-pipe first-input last-output config #:key (search-path? #t))
    "port/string/any port/string/any (#(symbol symbol string/(string:executable string ...)/procedure:{false/string false/string -> string/(string:executable string ...)}) ...) -> (integer:pid ...)
    first-input last-output (#(port/path/nothing port/path/nothing path/(path argument ...)/procedure:{path-in path-out -> execute-arguments} ...) ...) -> (integer:pid ...)
    like process-chain but uses path-pipe-chain internally and so allows automatically created paths (named pipes) between processes.
    example:
    (path-pipe-process-chain #f (current-output-port)
        (list
        (vector (q nothing) (q port) (list \"echo\" \"test\"))
        (vector (q path) (q path) (l (in out) (\"program\" \"--from\" in \"--to\" out)))))
    expected result:
    assuming that \"program\" reads from in and writes to out, \"test\" should appear on the current output port"
    (let*
      ( (config-length (length config)) (error (current-error-port))
        (pids
          (apply path-pipe-chain first-input
            last-output
            (debug-log
              (map
                (l (a)
                  (vector (vector-ref a 0) (vector-ref a 1)
                    (let (b (vector-ref a 2))
                      (l (in out)
                        (let
                          (executable-and-arguments
                            (any->list
                              (if (procedure? b) (b (and (string? in) in) (and (string? out) out))
                                b)))
                          (if (procedure? out)

                            )
                          (begin-first
                            (process-create (first executable-and-arguments)
                              (tail executable-and-arguments) (and (port? in) in)
                              (and (port? out) out) error #:search-path? search-path?)
                            (if (and (port? in) (not (eq? first-input in))) (close in))
                            (if (and (port? out) (not (eq? last-output out))) (close out))))))))
                config)))))
      (if (= (length pids) config-length) pids (begin (each (l (a) (kill a SIGTERM)) pids) (list))))))
