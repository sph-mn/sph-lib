(sc-comment
  "code for creating a guile extension as a shared library.
  for features that can not adequately be written in guile scheme, for example child process creation")

(sc-comment "set gnu source to include functions that arent part of the c standard (dirfd)")
(pre-define _GNU_SOURCE #t)

(pre-include
  "dirent.h"
  "sys/types.h" "sys/stat.h" "fcntl.h" "dirent.h" "unistd.h" "errno.h" "linux/limits.h" "libguile.h")

(pre-define-if-not-defined OPEN-MAX 1024)

(pre-define
  imht-set-key-t int
  imht-set-can-contain-zero? #f)

(sc-comment "include imht-set to use for tracking file descriptors to keep after fork")
(sc-include "foreign/imht-set")

(pre-define (debug-log format ...)
  (fprintf stderr (pre-string-concat "%s:%d " format "\n") __func__ __LINE__ __VA_ARGS__)
  (move-fd a)
  (do-while (= errno EINTR)
    (set a (dup a)))
  (dup2-fd old new)
  (begin
    (do-while (= errno EINTR)
      (dup2 old new))
    (close old))
  (close-fd scm a)
  (if (= scm SCM-BOOL-F) (close a))
  (ensure-fd a open-flags path)
  (begin
    "variable integer null/path ->
    if \"a\" is -1, set it to a newly opened filed descriptor for path or /dev/null"
    (if (= -1 a) (set a (open (if* path path "/dev/null") open-flags)))))

(pre-define (port-argument-set-fd a fd path)
  (begin
    "SCM int-variable char*-variable ->
    set fd to a file descriptor from an SCM argument or -1.
    if \"a\" is a path string, set \"path\""
    (if (scm-is-true (scm-port? a))
      (set fd (scm->int (scm-fileno a)))
      (if (scm-is-integer a)
        (set fd (scm->int a))
        (begin
          (set fd -1)
          (if (scm-is-string a) (set path (scm->locale-string a))))))))

(pre-define (set-standard-streams input output error)
  (begin
    "integer integer integer ->
    to be called in a new process"
    (if (> input 0)
      (begin
        (if (= 0 output) (move-fd output))
        (if (= 0 error) (move-fd error))
        (dup2-fd input 0)))
    (if (> output 1)
      (begin
        (if (= 1 error) (move-fd error))
        (dup2-fd output 1)))
    (if (> error 2) (dup2-fd error 2))))

(define (close-file-descriptors-from start-fd keep) (void int imht-set-t*)
  "integer imht-set ->
  try to close all used file descriptors greater than or equal to start-fd.
  tries to use one of /proc/{process-id}/fd, sysconf and getdtablesize.
  if none of those is available, closes file descriptors from sart-fd
  to 1024 or OPEN_MAX if that as defined at compile time"
  (define
    fd long
    maxfd long)
  (define-array path-proc-fd char (PATH_MAX))
  (define
    directory DIR*
    path-length int
    entry
    (struct
      dirent*)
    first-invalid char*)
  (set path-length
    (snprintf path-proc-fd (sizeof path-proc-fd) "/proc/%ld/fd" (convert-type (getpid) long)))
  (if
    (and
      (> path-length 0)
      (<= (convert-type path-length size-t) (sizeof path-proc-fd))
      (set directory (opendir path-proc-fd)))
    (begin
      (while (not (= 0 (set entry (readdir directory))))
        (set fd (strtol (struct-pointer-get entry d-name) (address-of first-invalid) 10))
        (if
          (and
            (not (= (struct-pointer-get entry d-name) first-invalid))
            (= (pointer-get first-invalid) 0)
            (>= fd 0)
            (< fd INT_MAX)
            (>= fd start-fd) (not (= fd (dirfd directory))) (not (imht-set-contains? keep fd)))
          (convert-type (close (convert-type fd int)) void)))
      (convert-type (closedir directory) void))
    ; fallback
    (begin
      (pre-if HAVE-SYSCONF (set maxfd (sysconf _SC-OPEN-MAX)) (set maxfd (getdtablesize)))
      (if (< maxfd 0) (set maxfd OPEN-MAX))
      (set fd start-fd)
      (while (<= fd maxfd)
        (if (not (imht-set-contains? keep fd)) (convert-type (close (convert-type fd int)) void))
        (set fd (+ 1 fd))))))

(define (free-env a) (void char**)
  "free a null pointer terminated char**"
  (define a-temp char** a)
  (while a-temp
    (free a-temp))
  (free a))

(define (scm-list->imht-set scm-a result) (int SCM imht-set-t**)
  (define a-length int (scm->uint32 (scm-length scm-a)))
  (if (not (imht-set-create (+ 3 a-length) result)) (return 1))
  (while (not (scm-is-null scm-a))
    (if (not (imht-set-add (pointer-get result) (scm->int (SCM-CAR scm-a))))
      (begin
        (imht-set-destroy (pointer-get result))
        (return 2)))
    (set scm-a (SCM-CDR scm-a)))
  (return 0))

(define (scm-string-list->string-pointer-array scm-a) (char** SCM)
  "returns a null pointer terminated char**"
  (define a-length int (scm->int (scm-length scm-a)))
  (define result char** (malloc (* (sizeof char*) (+ 1 a-length))))
  (set (pointer-get result a-length) 0)
  (define result-pointer char** result)
  (while (not (scm-is-null scm-a))
    (define
      b char*
      b-length size-t)
    (set
      b (scm->locale-stringn (SCM-CAR scm-a) (address-of b-length))
      (pointer-get result-pointer) (malloc (* (+ 1 b-length) (sizeof char))))
    (memcpy (pointer-get result-pointer) b b-length)
    (set
      (pointer-get (pointer-get result-pointer) b-length) 0
      result-pointer (+ 1 result-pointer)
      scm-a (SCM-CDR scm-a)))
  (return result))

(define
  (scm-primitive-process-create
    scm-executable
    scm-arguments
    scm-input-port scm-output-port scm-error-port scm-env scm-keep-descriptors scm-path-open-flags)
  (SCM SCM SCM SCM SCM SCM SCM SCM SCM)
  "see init-sph-lib for documentation"
  (define path-open-flags int
    (if* (scm-is-true scm-path-open-flags) (scm->int scm-path-open-flags) 0))
  (define arguments char** (scm-string-list->string-pointer-array scm-arguments))
  (define env char**
    (if* (= SCM-BOOL-F scm-env) environ (scm-string-list->string-pointer-array scm-env)))
  (define executable char* (scm->locale-string scm-executable))
  (define keep imht-set-t*)
  (define
    input int
    output int
    error int)
  (define input-path char* 0)
  (define output-path char* 0)
  (define error-path char* 0)
  (port-argument-set-fd scm-input-port input input-path)
  (port-argument-set-fd scm-output-port output output-path)
  (port-argument-set-fd scm-error-port error error-path)
  (if (scm-list->imht-set scm-keep-descriptors (address-of keep)) (return SCM-BOOL-F))
  (define process-id int (fork))
  (if (not (= 0 process-id))
    (begin
      (free arguments)
      (free executable)
      (imht-set-destroy keep)
      (if (not (= SCM-BOOL-F scm-env)) (free-env env))
      (return (scm-from-int process-id))))
  ;after fork
  (ensure-fd input O_RDONLY input-path)
  (ensure-fd output (bit-or O_WRONLY O_CREAT path-open-flags) output-path)
  (ensure-fd error (bit-or O_WRONLY O_CREAT path-open-flags) error-path)
  (imht-set-add keep input)
  (imht-set-add keep output)
  (imht-set-add keep error)
  (close-file-descriptors-from 3 keep)
  (set-standard-streams input output error)
  (execve executable arguments env)
  ; terminates the program immediately with neither scheme-level nor c-level cleanups
  (_exit 127)
  (return SCM-UNSPECIFIED))

(define (init-sph-lib) void
  (define primitive-process-create SCM
    (scm-c-define-gsubr "primitive-process-create" 8 0 0 scm-primitive-process-create))
  (scm-set-procedure-property!
    primitive-process-create
    (scm-from-locale-symbol "documentation")
    (scm-from-locale-string
      "string (string ...) false/port/string/integer false/port/string/integer false/port/string/integer false/(string ...) (integer ...) false/integer -> false/integer
      executable (argument ...) input output error environ-result keep-file-descriptors path-open-flags -> child-process-id
      values for input, output or error:
      * false: /dev/null
      * string: filesystem path
      * integer: file descriptor
      * port: port
      creates a child process via an async safe fork/exec.
      uses execve and does not search in directories of the PATH environment variable")))
