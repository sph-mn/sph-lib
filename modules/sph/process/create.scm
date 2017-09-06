(library (sph process create)
  (export
    execute-with-pipes
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
    (sph exception)
    (sph io)
    (sph io path-pipe-chain)
    (sph process)
    (only (sph alist) list->alist)
    (only (sph list)
      any->list
      compact
      first-or-false
      pair-reverse)
    (only (sph one) search-env-path-one begin-first))

  (define sph-process-create-description "create child processes and process chains")
  (load-extension "libguile-sph-lib" "init_sph_lib")
  (define-syntax-rule (boolean->integer a) (if a 1 0))

  (define*
    (process-create executable #:optional (arguments (list)) input-port output-port error-port
      #:key
      env
      (keep-descriptors (list))
      search-path?
      path-open-flags)
    "string:path/file-name [(string ...) port/string/integer/false ... #:key (env (environ)) (keep-descriptors (list)) search-path? (path-open-flags integer)] -> process-id
     \"executable\" is the path or file name of a file to execute to become the new process.
     if the given string for \"executable\" does not start with a slash and search-path? is true (default is false), it is searched in the directories in the PATH environment variable.
     the optional parameters are to set the standard streams.
     with the key parameters the environment variables for the new process can be set in the format (environ) returns.
     no file descriptors from the parent process are transferred to the child except if listed in keep-descriptors or given using the input/output/error-port parameters.
     when a string is given as port argument, it is interpreted as a filesystem path and #:path-open-flags can be used to add to O_WRONLY|O_CREAT.
     returns the process id of the newly created child process or false if the process could not be created"
    (if search-path?
      (let
        (executable
          (if (string-prefix? "/" executable) executable (search-env-path-one executable)))
        (and executable (file-exists? executable)
          (primitive-process-create executable (pair (basename executable) arguments)
            input-port output-port error-port env keep-descriptors path-open-flags)))
      (and (file-exists? executable)
        (primitive-process-create executable (pair (basename executable) arguments)
          input-port output-port error-port env keep-descriptors path-open-flags))))

  ; the following definitions are not directly dependent on the loaded extension
  ;

  (define* (process-chain first-input last-output execute-arguments #:key search-path?)
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
    (and (not (null? process-chain-result)) (every process-finish-success? process-chain-result)))

  (define (process-chain->string first-input . process-chain-arguments)
    "false/port any ... -> string"
    (call-with-pipe
      (l (in out)
        (let (pids (apply process-chain first-input out process-chain-arguments)) (close out)
          (begin-first (port->string in) (process-chain-finish pids))))))

  (define*
    (execute-with-pipes proc path arguments #:optional input? output? error? #:key search-path?)
    "procedure:{port ... -> any} string list boolean boolean boolean -> any:proc-result integer:exit-status
     executes a program and calls proc with pipes to or from the standard streams depending on if input/output/error are true.
     path is a filesystem path to an executable.
     you might have to close ports for proc to return.
     pipe ports are closed when after proc returns.
     example:
     (execute-with-pipes (l (input error) #t) \"/usr/bin/echo\" (list \"test\") #t #f #t)"
    (call-with-pipes
      (+ (boolean->integer input?) (boolean->integer output?) (boolean->integer error?))
      (l* pipes
        (let*
          ( (ports
              (let loop
                ((pipes (list->alist pipes)) (ports (list input? output? error?)) (first? #t))
                ; ((in . out) ...) (boolean boolean boolean) -> ((child-port . proc-port) ...)
                (if (null? ports) (list)
                  (if (first ports)
                    (pair
                      (if first?
                        ; the first of ports is an child-in/proc-out combination
                        (first pipes) (pair-reverse (first pipes)))
                      (loop (tail pipes) (tail ports) #f))
                    (pair (pair #f #f) (loop pipes (tail ports) #f))))))
            (child-ports (map first ports)) (proc-ports (compact (map tail ports)))
            (close-ports (nullary (each close-port proc-ports)))
            (pid
              (apply process-create path
                arguments (append child-ports (list #:search-path? search-path?)))))
          (each close-port (compact child-ports))
          (if pid
            (exception-always (close-ports)
              (let (result (apply proc proc-ports)) (values result (process-finish pid))))
            (begin (close-ports) (values #f #f)))))))

  (define* (process-chain-path-pipe first-input last-output config #:key search-path?)
    "port/string/any port/string/any (#(symbol symbol string/(string:executable string ...)/procedure:{false/string false/string -> string/(string:executable string ...)}) ...) -> (integer:pid ...)
     first-input last-output (#(port/path/nothing port/path/nothing path/(path argument ...)/procedure:{path-in path-out -> execute-arguments} ...) ...) -> (integer:pid ...)
     like process-chain but uses path-pipe-chain internally and so allows automatically created paths (named pipes) between processes.
     example:
     (path-pipe-process-chain #f (current-output-port)
       (list
         (vector (q nothing) (q port) (list \"echo\" \"test\"))
         (vector (q path) (q path) (l (in out) (\"program\" \"--from\" in \"--to\" out)))))
     expected result:
     assuming that \"program\" reads from file in and writes to file out, \"test\" should appear on the current output port"
    (let*
      ( (config-length (length config)) (error (current-error-port))
        (pids
          (apply path-pipe-chain first-input
            last-output
            (map
              (l (a)
                (vector (vector-ref a 0) (vector-ref a 1)
                  (let (b (vector-ref a 2))
                    (l (in out)
                      (let
                        (executable-and-arguments
                          (any->list
                            (if (procedure? b) (b (and (string? in) in) (and (string? out) out)) b)))
                        (begin-first
                          (process-create (first executable-and-arguments)
                            (tail executable-and-arguments)
                            (if (port? in) in (and (procedure? in) (in)))
                            (if (port? out) out (and (procedure? out) (out))) error
                            #:search-path? search-path?)
                          (if (and (port? in) (not (eq? first-input in))) (close in))
                          (if (and (port? out) (not (eq? last-output out))) (close out))))))))
              config))))
      (if (= (length pids) config-length) pids (begin (each (l (a) (kill a SIGTERM)) pids) (list))))))
