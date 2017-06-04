(library (sph process create)
  (export
    process-chain
    process-chain-finished-successfully?
    process-chain-paths/pipes
    process-chain-pipes->string
    process-create
    process-primitive-create-chain-with-pipes
    sph-process-create-description)
  (import
    (guile)
    (sph)
    (sph read-write)
    (only (sph list) any->list first-or-false)
    (only (sph one) search-env-path begin-first))

  (define sph-process-create-description
    "create child processes and process chains")

  (load-extension "libguile-sph-lib" "init_sph_lib")

  (define*
    (process-create executable #:optional (arguments (list)) input-port output-port error-port
      #:key
      env
      (keep-descriptors (list))
      search-path?)
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

  (define* (process-chain first-input last-output execute-arguments #:key search-path?)
    "port/any port/any (string/(string:executable string:argument ...) ...) -> (integer:pid ...)
    creates a new process for each execute-argument and sets standard input and output of the processes in a chaining manner:
    input is the first-input or the input from the previous process, the output is the output to the next process or the last-output.
    error port for each process is the current-error-port of the process calling process-chain pipes.
    if any process could not be created then all previously created processed are sent SIGTERM and the result is an empty list"
    (let*
      ( (execute-arguments-length (length execute-arguments)) (error (current-error-port))
        (pids
          (apply rw-pipe-chain first-input
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

  #;(
  (define (process-chain-pipes->string input . process-handler) "any procedure/string:command ->"
    (call-with-pipe
      (l (in out) (apply process-chain-pipes input out process-handler)
        (close out) (port->string in))))

  (define (chain-paths/pipes-call-with-destination source destination-type proc)
    "any symbol procedure:{any string/port -> any} -> (integer:pid . string/port:next-input)"
    (if (type-path? destination-type)
      (let (destination-path (create-temp-fifo))
        (list (process-create (thunk (proc source destination-path)) (port-or-true source))
          destination-path))
      (let (ports (pipe))
        (list
          (process-create (thunk (close (first ports)) (proc source (tail ports)))
            (port-or-true source) (tail ports))
          (begin (close (tail ports)) (first ports))))))

  (define-syntax-rule (port-or-true a) (or (and (port? a) a) #t))

  (define (chain-paths/pipes-call-with-destination-last source destination destination-type proc)
    "any any symbol procedure:{any any ->} -> integer:process-id/any"
    (if (type-path? destination-type)
      (if (port? destination)
        (let (destination-path (create-temp-fifo))
          (process-create (thunk (proc source destination-path)) (port-or-true source))
          (process-create
            (thunk
              (call-with-input-file destination-path (l (port) (port-copy-all port destination)))
              (delete-file destination-path))))
        (process-create (thunk (proc source destination)) (port-or-true source)))
      (if (string? destination)
        (process-create
          (thunk
            (let (destination (open destination (logior O_WRONLY O_CREAT)))
              (proc source destination)))
          (port-or-true source))
        (process-create (thunk (proc source destination)) (port-or-true source)
          (port-or-true destination)))))

  (define (process-chain-finished-successfully? result-list)
    "(integer:pid ...) -> boolean
    wait for the termination of the processes and check if its exit status is 0"
    (every process-finished-successfully? result-list))

  (define (chain-paths/pipes-call-with-source source source-type proc)
    "-> integer:pid/unspecified
    if the result is not a pid, then the suprocess is already finished.
    this procedure creates the result for a process-chain-paths/pipes loop iteration"
    (if source
      (if (string? source)
        (let (pid (if (type-path? source-type) (proc source) (call-with-input-file source proc)))
          (delete-file source) pid)
        (if (type-path? source-type)
          (let (source-path (create-temp-fifo))
            ;currently in this case it is necessary that proc creates a sub-process
            (let (pid (proc source-path)) (port->file source source-path)
              (close source)
              ;and no stream processing in this case, because it is not yet clear how to handle this
              (delete-file source-path) pid))
          (first-as-result (proc source) (close source))))
      (proc source)))

  (define (chain-paths/pipes-call-with-source-first source source-type proc)
    "any symbol procedure:{any:source ->} -> any
    sets up input output ports and calls \"proc\""
    (if (string? source)
      (if (type-path? source-type) (proc source) (call-with-input-file source proc))
      (if (and (port? source) (type-path? source-type))
        (let (path (create-temp-fifo)) (proc path) (port->file source path)) (proc source))))

  (define (get-port a port-default) (if (boolean? a) port-default a))

  (define (process-chain-paths/pipes source destination . proc-config)
    "::
     string:file-path/port/true:standard-input/any
     string:file-path/port/true:standard-output/any
     (symbol:path/port/any symbol:path/port/any procedure:{port/string/false:source port/string/false:destination}) ...
     ->
     (integer:process-pid/proc-result ...)

     similar to process-chain-pipes, but also supports temporary intermedia file paths as input/output arguments (the paths point to named pipes).
     procedures of varying type signatures can be used for processes, processes that read from or write to files, or that read from or write to ports, all types possibly mixed.
     procedures of proc-config are applied in series in separate processes with source and destination ports in a data-flow chaining manner, like using | with bash.
     the source and destination arguments are made compatible automatically, with their expected input and output types specified by the first two symbols of a proc-config element.
     the procedure arguments may be file-paths, named-pipe-paths or unnamed-pipes/ports. the type of the first source and last destination is completely unrestricted and can be anything.
     as an example, you could connect a program that reads from a file and writes to standard out with a program that also reads from a file and writes to a port with arguments like this:
     (#f final-destination-port (list (q any) (q port) (l (input port) (file->port \"test\" port))) (list (q path) (q port) (l (path port) (do-stuff path port))))
    note: this procedure does not wait until all processes have finished. you can use \"waitpid\" or \"process-chain-finished-successfully?\" to archieve that. (relevant for example when
    processes write to a tty, the program would exit but new output appears after the new prompt)"
    ;debugging tip: look at the port types (input/output) that are passed to process-create by displaying the port objects
    (if (null? proc-config) (list)
      ;"tail" skips the first #f pid
      (tail
        (let-syntax ((get-port (syntax-rule (a default) (if (and a (boolean? a)) default a))))
          (apply
            (rec (loop is-first pid source config . rest)
              (pair pid
                (apply
                  (l (source-type destination-type proc)
                    ( (if is-first chain-paths/pipes-call-with-source-first
                        chain-paths/pipes-call-with-source)
                      source source-type
                      (l (source)
                        (if (null? rest)
                          (list
                            (chain-paths/pipes-call-with-destination-last source
                              (get-port destination (current-output-port)) destination-type proc))
                          (apply loop #f
                            (append
                              (chain-paths/pipes-call-with-destination source destination-type proc)
                              rest))))))
                  config)))
  #t #f (get-port source (current-input-port)) proc-config)))))

  ))
