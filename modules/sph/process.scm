; (sph process) - process creation, management and inter-process communication.
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
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
    execlp-new-process
    execute
    execute+check-result
    execute->file
    execute->string
    execute-and
    execute-with-pipe
    primitive-process-create-chain-with-pipes
    process-chain-finished-successfully?
    process-create
    process-create-chain-with-paths/pipes
    process-create-chain-with-pipes
    process-create-chain-with-pipes->string
    process-primitive-create-chain-with-pipes
    process-replace
    process-replace-with-environment
    process-replace-without-search
    process-replace-without-search-with-environment
    process-set-standard-ports
    shell-eval
    shell-eval+check-result
    shell-eval->string)
  (import
    (guile)
    (ice-9 popen)
    (rnrs base)
    (rnrs io ports)
    (sph)
    (sph one)
    (sph read-write)
    (srfi srfi-31)
    (only (sph conditional) false-if)
    (only (sph list) tail-ref)
    (only (srfi srfi-1) drop-right))

  (define execute system*)
  (define shell-eval system)

  (define (call-with-working-directory path p)
    (let ((cwd (getcwd)) (r (begin (chdir path) (p)))) (chdir cwd) r))

  (define (process-replace-without-search program-path . args)
    "replaces the current process image with the execution of the program at program-path.
    executes programs conventionally with program-path as the first argument"
    (apply execl program-path (basename program-path) args))

  (define (process-replace program-name/path . args)
    "like process-replace-without-search except that if the path to the program to execute does not start with a slash
    it is searched in the directories in the PATH environment variable"
    (apply execlp program-name/path (basename program-name/path) args))

  (define (process-replace-without-search-with-environment env path . args)
    "(string:\"name=value\" ...) string string ... ->
    like process-replace-without-search but the environment variables of the process are passed with the env parameter.
    simpler interface to execle.
    to use the current environment variables the (environ) procedure can be used - it creates output in the expected format for \"env\""
    (apply execle path env (basename path) args))

  (define (process-replace-with-environment env name/path . args)
    "(string:\"name=value\" ...) string string ... ->
    like process-replace-without-search-with-environment except that if the path to the program to execute does not start with a slash
    it is searched in the directories in the PATH environment variable"
    (apply execle (if (string-contains name/path "/") name/path (search-env-path name/path))
      env (basename name/path) args))

  (define (get-file-descriptor port mode default)
    (or (and port (false-if-exception (fileno (if (boolean? port) default port))))
      (open-fdes *null-device* mode)))

  (define (process-set-standard-ports input-port output-port error-port)
    "initialises standard input/output/error ports for process-chains. used in create-chain-with-pipes"
    (let
      ( (input-fd (get-file-descriptor input-port O_RDONLY (current-input-port)))
        (output-fd (get-file-descriptor output-port O_WRONLY (current-output-port)))
        (error-fd (get-file-descriptor error-port O_WRONLY (current-error-port))))
      (if (not (= input-fd 0))
        (begin (if (= output-fd 0) (set! output-fd (dup->fdes 0)))
          (if (= error-fd 0) (set! error-fd (dup->fdes 0))) (dup2 input-fd 0)))
      (if (not (= output-fd 1))
        (begin (if (= error-fd 1) (set! error-fd (dup->fdes 1))) (dup2 output-fd 1)))
      (dup2 error-fd 2)))

  (define* (process-create content #:optional (input-port #t) (output-port #t) (error-port #t))
    "procedure:{-> integer:exit-code} port/false port/false port/false -> process-id
    apply zero parameter procedure content in a background copied process image created by fork and result in
    the childs process-id in the foreground process. the optional arguments set the standard-ports for the new process"
    (let (process-id (primitive-fork))
      (if (= 0 process-id)
        (begin (process-set-standard-ports input-port output-port error-port) (exit (content)))
        process-id)))

  (define process-primitive-create-chain-with-pipes
    (letrec
      ( (loop
          (l (inp last-out proc . rest)
            (if (null? rest) (process-create proc inp last-out)
              (let (ports (pipe))
                (process-create (thunk (close (first ports)) (proc)) inp (tail ports))
                (close (tail ports)) (apply loop (first ports) last-out rest))))))
      (l (input-port output-port . proc)
        "each proc is executed in a separate process. the standard input and output of the processes are linked by pipes"
        (if (not (null? proc))
          (apply loop (if (and input-port (boolean? input-port)) (current-input-port) input-port)
            (if (and output-port (boolean? output-port)) (current-output-port) output-port) proc)))))

  (define (command/proc->procedure a)
    (if (procedure? a) a (if (list? a) (thunk (apply execute a)) (thunk (execute a)))))

  (define process-create-chain-with-pipes
    (l (input-port output-port . command/proc)
      "port/true/any port/true/any procedure/(string ...)/string ->
        like process-primitive-create-chain-with-pipes but also supports lists as value for command/proc which contain arguments for \"execute\".
        the first execute argument is automatically also the second argument to follow the common execv calling convention.
        procedure: {->}"
      (apply process-primitive-create-chain-with-pipes input-port
        output-port (map command/proc->procedure command/proc))))

  (define (execute-with-pipe proc mode command . arguments)
    "procedure integer string list ->
    execute a program like with a pipe connected to its standard-output or standard-input and pass it as a port to \"proc\".
    mode can be one of the guile variables OPEN_READ OPEN_WRITE OPEN_BOTH"
    (let* ((port (apply open-pipe* mode command arguments)) (r (proc port))) (close-pipe port) r))

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

  (define (process-create-chain-with-pipes->string input . args)
    (call-with-pipe
      (l (in out) (apply process-create-chain-with-pipes input out args)
        (close out) (port->string in))))

  (define (execute+check-result name . options)
    "string (string ...) -> boolean
    execute command with system*, check exit-status and return true if it is zero, false otherwise."
    (let ((status (status:exit-val (apply system* name options))))
      (true? (and status (= status 0)))))

  (define (shell-eval+check-result command-str)
    "string -> boolean
    execute command with system, check exit-status and return true if it is zero, false otherwise."
    (let ((status (status:exit-val (system command-str)))) (true? (and status (= status 0)))))

  (define-syntax-rule (type-path? a) (eqv? (q path) a))

  (define (process-chain-finished-successfully? pid)
    "integer/any -> integer/boolean
    if pid is an integer, wait for the termination of the process and check if its exit status is 0.
    if pid is anything else, the result is pid"
    (if (integer? pid) (= 0 (status:exit-val (tail (waitpid pid)))) pid))

  (define (chain-with-paths/pipes-call-with-source source source-type proc)
    "-> integer:pid/unspecified
    if the result is not a pid, then the suprocess is already finished"
    ;this procedure creates the result for a process-create-chain-with-paths/pipes loop iteration
    (if source
      (if (string? source)
        (let (pid (if (type-path? source-type) (proc source) (call-with-input-file source proc)))
          (if (integer? pid) (waitpid pid)) (delete-file source))
        (if (type-path? source-type)
          (let (source-path (create-temp-fifo))
            ;currently in this case it is necessary that proc creates a sub-process
            (let (pid (proc source-path)) (port->file source source-path)
              (close source)
              ;and no stream processing in this case, because it is not yet clear how to handle this
              (first-as-result (process-chain-finished-successfully? pid) (delete-file source-path))))
          (first-as-result (proc source) (close source))))
      (proc source)))

  (define (chain-with-paths/pipes-call-with-source-first source source-type proc)
    "any symbol procedure:{any:source ->} -> any"
    (if (string? source)
      (if (type-path? source-type) (proc source) (call-with-input-file source proc))
      (if (and (port? source) (type-path? source-type))
        (let (path (create-temp-fifo)) (proc path) (port->file source path)) (proc source))))

  (define (chain-with-paths/pipes-call-with-target source target-type proc)
    "any symbol procedure:{any string/port -> any} -> string/port:next-input"
    (if (type-path? target-type)
      (let (target-path (create-temp-fifo))
        (process-create (thunk (proc source target-path)) (port-or-true source)) target-path)
      (let (ports (pipe))
        (process-create (thunk (close (first ports)) (proc source (tail ports)))
          (port-or-true source) (tail ports))
        (close (tail ports)) (first ports))))

  (define-syntax-rule (port-or-true a) (or (and (port? a) a) #t))

  (define (chain-with-paths/pipes-call-with-target-last source target target-type proc)
    "any any symbol procedure:{any any ->} -> integer:process-id/any"
    (if (type-path? target-type)
      (if (port? target)
        (let (target-path (create-temp-fifo))
          (process-create (thunk (proc source target-path)) (port-or-true source))
          (process-create
            (thunk (call-with-input-file target-path (l (port) (port-copy-all port target)))
              (delete-file target-path))))
        (process-create (thunk (proc source target)) (port-or-true source)))
      (if (string? target)
        (process-create
          (thunk (let (target (open target (logior O_WRONLY O_CREAT))) (proc source target)))
          (port-or-true source))
        (process-create (thunk (proc source target)) (port-or-true source) (port-or-true target)))))

  (define (process-create-chain-with-paths/pipes source target . proc-config)
    "::
     string:file-path/port/boolean-true:standard-input/any
     string:file-path/port/boolean-true:standard-output/any
     (symbol:path/port/any symbol:path/port/any procedure:{source target}) ...
     ->
     integer:last-process-pid/unspecified

     similar to process-create-chain-with-pipes, but also supports intermediate file paths in the form of automatically created temporary named-pipes.
     procedures of varying type signatures can be used for processes - processes that read from or write to files, or processes that read from or write to ports, all mixed.
     procedures of proc-config are applied in series in separate processes with source and target ports in a data-flow chaining manner, like using | with bash.
     the source and target arguments are made compatible automatically, with their expected input and output types specified by the first two symbols of a proc-config element.
     the procedure arguments may be file-paths, named-pipes or unnamed-pipes/ports. the type of the first source and last target is completely unrestricted and can be anything.
     as an example, you could connect a program that reads from a file and writes to standard out with a program that also reads from a file and writes to a port with arguments like this:
     (#f target-port (list (q any) (q port) (\"cat\" filename)) (list (q path) (q port) (l (path port) (do-stuff path port))))
    note: this procedure does not wait until the last process has finished. you can use \"waitpid\" or \"process-chain-finished-successfully?\" to archieve that. (relevant for example when
    processes write to a tty, the program exits but new output appears after the new prompt)"
    ;debugging tip: look at the port types (input/output) that are passed to process-create by displaying the port objects
    (if (null? proc-config) #t
      (let-syntax ((get-port (syntax-rule (a default) (if (and a (boolean? a)) default a))))
        (apply
          (rec (loop is-first source config . rest)
            (apply
              (l (source-type target-type proc)
                ( (if is-first chain-with-paths/pipes-call-with-source-first
                    chain-with-paths/pipes-call-with-source)
                  source source-type
                  (l (source)
                    (if (null? rest)
                      (chain-with-paths/pipes-call-with-target-last source
                        (get-port target (current-output-port)) target-type proc)
                      (apply loop #f
                        (chain-with-paths/pipes-call-with-target source target-type proc) rest)))))
              config))
          #t (get-port source (current-input-port)) proc-config))))

  (define (execute-and a . rest)
    "(string ...) ... -> system*-result
     takes lists of arguments to system* and calls system* for each of these arguments.
     if one call returns with a non-zero exit value the processing stops and returns the result of system*"
    (let loop ((rest rest) (result (apply system* a)))
      (if (null? rest) result
        (if (= 0 (status:exit-val result)) (loop (tail rest) (apply system* (first rest))))))))