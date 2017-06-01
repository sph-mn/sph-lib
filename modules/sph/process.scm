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
    process-eval
    process-finished-successfully?
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
    (sph filesystem)
    (sph one)
    (sph read-write)
    (srfi srfi-31))

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

  (define (execute-with-pipe proc mode path . arguments)
    "procedure integer string list -> any
    execute a program with a pipe connected to its standard-output and/or standard-input and pass it as one port to \"proc\".
    mode can be one of the guile variables OPEN_READ OPEN_WRITE OPEN_BOTH"
    (let* ((port (apply open-pipe* mode path arguments)) (r (proc port))) (close-pipe port) r))

  (define (process-eval code proc)
    "scheme-datum procedure:{process-output-port -> any} -> any
    evaluate code in a new guile process, independent from the current process (no shared environment)
    and pass a port for the standard output of the process to proc"
    (call-with-temporary-file
      (l (file) (write code file)
        (fsync file)
        (execute-with-pipe proc OPEN_READ "guile" "--no-auto-compile" (port-filename file)))))

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

  (define (exit-value-zero? system-result) (zero? (status:exit-val system-result)))

  (define (execute+check-result path . arguments)
    "string (string ...) -> boolean
    execute command with system*, check exit-status and return true if it is zero, false otherwise."
    (let ((status (status:exit-val (apply system* path arguments))))
      (not (not (and status (= status 0))))))

  (define (shell-eval+check-result command-str)
    "string -> boolean
    execute command with system, check exit-status and return true if it is zero, false otherwise."
    (let ((status (status:exit-val (system command-str)))) (not (not (and status (= status 0))))))

  (define (process-finished-successfully? pid)
    "integer -> boolean
    wait for the termination of the process identified by the given process id and check if its exit status is 0"
    (= 0 (status:exit-val (tail (waitpid pid)))))

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
