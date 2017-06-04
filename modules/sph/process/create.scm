(library (sph process create)
  (export
    process-chain
    process-chain->string
    process-chain-finish
    process-chain-finish-success?
    process-create
    sph-process-create-description)
  (import
    (guile)
    (sph)
    (sph process)
    (sph io)
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
  (define-syntax-rule (type-path? a) (eqv? (q path) a))

  (define* (process-chain first-input last-output execute-arguments #:key (search-path? #t))
    "port/any port/any (string/(string:executable string:argument ...) ...) -> (integer:pid ...)
    creates a new process for each execute-argument and sets standard input and output of the processes in a chaining manner:
    input is the first-input or the input from the previous process, the output is the output to the next process or the last-output.
    error port for each process is the current-error-port of the process calling process-chain pipes.
    if any process could not be created then all previously created processed are sent SIGTERM and the result is an empty list"
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
    (and process-chain-result (every process-finish? process-chain-result)))

  (define (process-chain-finish-success? process-chain-result)
    "false/(integer:pid ...) -> boolean
    wait for the termination of the processes and check if its exit status is 0"
    (and process-chain-result (every process-finish-success? process-chain-result)))

  (define (process-chain->string first-input . process-chain-arguments)
    "false/port any ... -> string"
    (call-with-pipe
      (l (in out)
        (let (pids (apply process-chain first-input out process-chain-arguments)) (close out)
          (begin-first (port->string in) (process-chain-finish pids)))))))
