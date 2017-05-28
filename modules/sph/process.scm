; (sph process) - process creation, management and inter-process communication.
; written for the guile scheme interpreter
; Copyright (C) 2010-2017 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph process)
  (export
    call-with-working-directory
    execute
    execute+check-result
    execute->file
    execute->port
    execute->string
    execute-and
    execute-with-pipe
    exit-value-zero?
    process-and
    process-chain-finished-successfully?
    process-chain-paths/pipes
    process-chain-pipes
    process-chain-pipes->string
    process-create
    process-finished-successfully?
    process-primitive-create-chain-with-pipes
    process-replace
    process-replace-e
    process-replace-p
    process-replace-pe
    shell-eval
    shell-eval+check-result
    shell-eval->string)
  (import
    (guile)
    (ice-9 popen)
    (ice-9 threads)
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph one)
    (sph read-write)
    (srfi srfi-31)
    (only (sph conditional) false-if)
    (only (srfi srfi-1) last))

  (define execute system*)
  (define shell-eval system)

  (define (call-with-working-directory path p)
    (let* ((cwd (getcwd)) (r (begin (chdir path) (p)))) (chdir cwd) r))

  (define (process-replace program-path . arguments)
    "replaces the current process image with the execution of the program at program-path.
    executes programs conventionally with program-path as the first argument.
    uses execl"
    (apply execl program-path (basename program-path) arguments))

  (define (process-replace-p program-name/path . arguments)
    "like process-replace-without-search except that if the path to the program to execute does not start with a slash
    it is searched in the directories in the PATH environment variable.
    uses execlp"
    (apply execlp program-name/path (basename program-name/path) arguments))

  (define (process-replace-e env path . arguments)
    "(string:\"name=value\" ...) string string ... ->
    like process-replace but the environment variables of the process are passed with the env parameter.
    to use the current environment variables the (environ) procedure can be used - it creates output in the expected format for \"env\".
    uses execle"
    (apply execle path env (basename path) arguments))

  (define (process-replace-pe env name/path . arguments)
    "(string:\"name=value\" ...) string string ... ->
    like process-replace-e except that if the path to the program to execute does not start with a slash
    it is searched in the directories in the PATH environment variable"
    (apply execle (if (string-prefix? "/" name/path) name/path (search-env-path name/path))
      env (basename name/path) arguments))

  (define (get-port a port-default) (if (boolean? a) port-default a))

  (define*
    (process-create executable #:optional (arguments (list)) input-port output-port error-port
      #:key
      prepare
      (env (environ)))
    "procedure string:path/file-name [list port/false port/false port/false list:environ-result] -> process-id
    \"executable\" is the path or file name of a file to execute to become the new process.
    if the given string for \"executable\" does not start with a slash, it is searched in the directories in the PATH environment variable.
    \"prepare\" is a thunk that will be called after fork just before the execve system call. it can be used for example to close file descriptors that have been copied by fork.
    the optional parameters are to set the standard streams and environment variables for the new process.
    by default the standard streams are not inherited from the parent process (they are closed in the child)"
    (let
      ( (new-i (and input-port (fileno input-port))) (new-o (and output-port (fileno output-port)))
        (new-e (and error-port (fileno error-port)))
        (executable (if (string-prefix? "/" executable) executable (search-env-path executable)))
        (first-argument (basename executable)))
      (let (process-id (primitive-fork))
        (if (= 0 process-id)
          (begin
            ;this branch is executed in the forked process.
            ;a fork copies only the current thread and state of previously existing threads might be inconsistent.
            ;because of that it is advised to call, if any, only "async-safe" functions until "exec".
            ;the following is supposed to do as little as possible - but ideally it would be implemented in c, but then c compilation is required for using this library.
            (let*
              ( (new-i (or new-i (open-fdes "/dev/null" O_RDONLY)))
                (new-o (or new-o (open-fdes "/dev/null" O_WRONLY)))
                (new-e (or new-e (open-fdes "/dev/null" O_WRONLY)))
                ;move conflicting descriptors
                (new-o (if (and (> new-i 0) (= 0 new-o)) (dup new-o) new-o))
                (new-e (if (and (> new-i 0) (= 0 new-e)) (dup new-e) new-e))
                (new-e (if (and (> new-o 1) (= 1 new-e)) (dup new-e) new-e)))
              ;if new-i is not already the inherited standard descriptor
              (if (> new-i 0)
                (begin
                  ;move the new output and error streams away from descriptor 0 if they use it
                  ;(dup2 old new) closes the new descriptor if necessary and makes the new one an alias for the old one.
                  ;this makes the desired stream the standard input of the new process
                  (dup2 new-i 0) (close-fdes new-i)))
              (if (> new-o 1) (begin (dup2 new-o 1) (close-fdes new-o)))
              (if (> new-e 2) (begin (dup2 new-e 2) (close-fdes new-e))))
            (and prepare (prepare))
            ;execle because it uses execve which posix requires to be async-safe in contrast to execv
            (apply execle executable env first-argument arguments))
          process-id))))

  (define process-chain-pipes
    (l (port-input port-output . command/proc)
      "port/true/any port/true/any procedure:{}/(string ...)/string -> (integer:pid/proc-result ...)
      creates a new process for each command/proc and sets the standard-input and -output of the processes in a chaining manner:
      the first input is the given input-port, the first output is the input of the next process, until the last port is the given output-port.
      supports lists as value for command/proc that contain arguments for \"execute\".
      the first program-path automatically becomes the second argument to follow the common execv calling convention"
      (apply rw-chain-pipes port-input
        port-output
        (map
          (l (a)
            (l (pipe-in pipe-out in out) (if pipe-out (setvbuf pipe-out (q none)))
              (let
                (pid
                  (process-create
                    (thunk (if pipe-in (close-port pipe-in))
                      (if (procedure? a) (a)
                        (if (list? a) (apply process-replace a) (process-replace a))))
                    (or in pipe-in) (or out pipe-out)))
                (if pipe-out (close-port pipe-out)) pid)))
          command/proc))))

  (define (execute-with-pipe proc mode path . arguments)
    "procedure integer string list -> any
    execute a program with a pipe connected to its standard-output and/or standard-input and pass it as a port to \"proc\".
    mode can be one of the guile variables OPEN_READ OPEN_WRITE OPEN_BOTH"
    (let* ((port (apply open-pipe* mode path arguments)) (r (proc port))) (close-pipe port) r))

  (define (execute->port port path . arguments)
    "port string string ... ->
    execute program at \"path\" with \"arguments\" and write everything that is written to standard output by the program to \"port\""
    (apply execute-with-pipe (l (port-program) (port-copy-all port-program port))
      OPEN_READ path arguments))

  (define (execute->file target-path command . command-arguments)
    "string (string ...) string ->
    apply \"system*\" with command and command-arguments and set standard-output to a file at target-path. the file is either created or overwritten"
    (apply execute-with-pipe
      (l (program)
        (call-with-output-file target-path
          (l (file)
            (let loop (octet (get-u8 program))
              (if (eof-object? octet) target-path
                (begin (put-u8 file octet) (loop (get-u8 program))))))))
      OPEN_READ command command-arguments))

  (define (execute->string command . command-arguments)
    "string (string ...) string ->
    apply \"system*\" with command and command-arguments and result in a string for the output the program wrote to standard output"
    (apply execute-with-pipe port->string OPEN_READ command command-arguments))

  (define (shell-eval->string command-str)
    "string -> string
    evaluate command-string with sh and return the resulting string"
    (let* ((port (open-pipe command-str OPEN_READ)) (result-str (port->string port)))
      (close-pipe port) result-str))

  (define (process-chain-pipes->string input . process-handler) "any procedure/string:command ->"
    (call-with-pipe
      (l (in out) (apply process-chain-pipes input out process-handler)
        (close out) (port->string in))))

  (define (exit-value-zero? system-result) (zero? (status:exit-val system-result)))

  (define (execute+check-result path . arguments)
    "string (string ...) -> boolean
    execute command with system*, check exit-status and return true if it is zero, false otherwise."
    (let ((status (status:exit-val (apply system* path arguments))))
      (true? (and status (= status 0)))))

  (define (shell-eval+check-result command-str)
    "string -> boolean
    execute command with system, check exit-status and return true if it is zero, false otherwise."
    (let ((status (status:exit-val (system command-str)))) (true? (and status (= status 0)))))

  (define-syntax-rule (type-path? a) (eqv? (q path) a))

  (define (process-finished-successfully? pid)
    "integer -> boolean
    wait for the termination of the process identified by the given process id and check if its exit status is 0"
    (= 0 (status:exit-val (tail (waitpid pid)))))

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

  (define (execute-and a . rest)
    "(string ...) ... -> system*-result
     takes lists of arguments to system* and calls system* for each of these arguments.
     if one call returns with a non-zero exit value the processing stops and returns the result of system*"
    (let loop ((rest rest) (result (apply system* a)))
      (if (null? rest) result
        (if (= 0 (status:exit-val result)) (loop (tail rest) (apply system* (first rest)))))))

  (define-syntax-rule (process-and create-exit-status ...)
    (and
      ( (lambda (status) (and (integer? status) (zero? status)))
        (status:exit-val create-exit-status))
      ...)))
