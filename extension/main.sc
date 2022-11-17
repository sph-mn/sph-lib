(sc-comment
  "code for creating a guile extension as a shared library.
   for features that can apparently not adequately be written in guile scheme, for example child process creation")

(sc-comment "set gnu source to include functions that arent part of the c standard (dirfd)")
(pre-define _GNU_SOURCE #t)

(pre-include "stdio.h" "string.h"
  "dirent.h" "sys/types.h" "sys/stat.h"
  "fcntl.h" "dirent.h" "unistd.h" "errno.h" "linux/limits.h" "libguile.h")

(pre-define-if-not-defined OPEN-MAX 1024)
(sc-comment "include sph-sc-lib sph/set.c to use for tracking file descriptors to keep after fork")
(sc-include "foreign/set")
(sph-set-declare-type-nonull fd-set int sph-set-hash-integer sph-set-equal-integer 0 2)

(pre-define
  (debug-log format ...)
  (fprintf stderr (pre-string-concat "%s:%d " format "\n") __func__ __LINE__ __VA_ARGS__)
  (move-fd a) (do-while (= errno EINTR) (set a (dup a)))
  (dup2-fd old new) (begin (do-while (= errno EINTR) (dup2 old new)) (close old))
  (close-fd scm a) (if (scm-is-false scm) (close a))
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
    (if (scm-is-true (scm-port? a)) (set fd (scm->int (scm-fileno a)))
      (if (scm-is-integer a) (set fd (scm->int a))
        (begin (set fd -1) (if (scm-is-string a) (set path (scm->locale-string a))))))))

(pre-define (set-standard-streams input output error)
  (begin
    "integer integer integer ->
     to be called in a new process"
    (if (> input 0)
      (begin (if (= 0 output) (move-fd output)) (if (= 0 error) (move-fd error)) (dup2-fd input 0)))
    (if (> output 1) (begin (if (= 1 error) (move-fd error)) (dup2-fd output 1)))
    (if (> error 2) (dup2-fd error 2))))

(define (close-file-descriptors-from start-fd keep) (void int fd-set-t)
  "integer fd-set ->
   try to close all used file descriptors greater than or equal to start-fd.
   tries to use one of /proc/{process-id}/fd, sysconf and getdtablesize.
   if none of those is available, closes file descriptors from start-fd
   to 1024 or OPEN_MAX if that is defined at compile time"
  (declare
    fd long
    maxfd long
    path-proc-fd (array char (PATH_MAX))
    directory DIR*
    path-length int
    entry (struct dirent*)
    first-invalid char*)
  (set path-length
    (snprintf path-proc-fd (sizeof path-proc-fd) "/proc/%ld/fd" (convert-type (getpid) long)))
  (if
    (and (> path-length 0) (<= (convert-type path-length size-t) (sizeof path-proc-fd))
      (set directory (opendir path-proc-fd)))
    (begin
      (while (not (= 0 (set entry (readdir directory))))
        (set fd (strtol entry:d-name &first-invalid 10))
        (if
          (and (not (= entry:d-name first-invalid)) (= *first-invalid 0)
            (>= fd 0) (< fd INT_MAX) (>= fd start-fd)
            (not (= fd (dirfd directory))) (not (fd-set-get keep fd)))
          (convert-type (close (convert-type fd int)) void)))
      (convert-type (closedir directory) void))
    (begin
      (sc-comment "fallback")
      (pre-if HAVE-SYSCONF (set maxfd (sysconf _SC-OPEN-MAX)) (set maxfd (getdtablesize)))
      (if (< maxfd 0) (set maxfd OPEN-MAX))
      (set fd start-fd)
      (while (<= fd maxfd)
        (if (not (fd-set-get keep fd)) (convert-type (close (convert-type fd int)) void))
        (set fd (+ 1 fd))))))

(define (free-env a) (void char**)
  "free a null pointer terminated char**"
  (define a-temp char** a)
  (while a-temp (free a-temp))
  (free a))

(define (scm-is-null-scm a) ((unsigned char) SCM)
  (return (scm-is-true (scm-call-1 (scm-variable-ref (scm-c-lookup "null?")) a))))

(define (scm-list->fd-set scm-a out) (int SCM fd-set-t*)
  (declare result fd-set-t)
  (define a-length int (scm->uint32 (scm-length scm-a)))
  (if (fd-set-new (+ 3 a-length) &result) (return 1))
  (while (not (scm-is-null-scm scm-a))
    (if (not (fd-set-add result (scm->int (SCM-CAR scm-a))))
      (begin (fd-set-free result) (return 2)))
    (set scm-a (SCM-CDR scm-a)))
  (set *out result)
  (return 0))

(define (scm-string-list->string-pointer-array scm-a) (char** SCM)
  "returns a null pointer terminated char**"
  (define a-length int (scm->int (scm-length scm-a)))
  (define result char** (malloc (* (sizeof char*) (+ 1 a-length))))
  (set (array-get result a-length) 0)
  (define result-pointer char** result)
  (while (not (scm-is-null-scm scm-a))
    (declare b char* b-length size-t)
    (set
      b (scm->locale-stringn (SCM-CAR scm-a) &b-length)
      *result-pointer (malloc (* (+ 1 b-length) (sizeof char))))
    (memcpy *result-pointer b b-length)
    (set (array-get *result-pointer b-length) 0)
    (set result-pointer (+ 1 result-pointer))
    (set scm-a (SCM-CDR scm-a)))
  (return result))

(define
  (scm-primitive-process-create scm-executable scm-arguments scm-input-port scm-output-port scm-error-port scm-env scm-keep-descriptors scm-path-open-flags)
  (SCM SCM SCM SCM SCM SCM SCM SCM SCM)
  (define path-open-flags int
    (if* (scm-is-true scm-path-open-flags) (scm->int scm-path-open-flags) 0))
  (define arguments char** (scm-string-list->string-pointer-array scm-arguments))
  (define env char**
    (if* (scm-is-false scm-env) environ (scm-string-list->string-pointer-array scm-env)))
  (define executable char* (scm->locale-string scm-executable))
  (declare keep fd-set-t input int output int error int)
  (define input-path char* 0)
  (define output-path char* 0)
  (define error-path char* 0)
  (port-argument-set-fd scm-input-port input input-path)
  (port-argument-set-fd scm-output-port output output-path)
  (port-argument-set-fd scm-error-port error error-path)
  (if (scm-list->fd-set scm-keep-descriptors &keep) (return SCM-BOOL-F))
  (define process-id int (fork))
  (if (not (= 0 process-id))
    (begin
      (free arguments)
      (free executable)
      (fd-set-free keep)
      (if (scm-is-true scm-env) (free-env env))
      (return (scm-from-int process-id))))
  (sc-comment "after fork")
  (ensure-fd input O_RDONLY input-path)
  (ensure-fd output (bit-or O_WRONLY O_CREAT path-open-flags) output-path)
  (ensure-fd error (bit-or O_WRONLY O_CREAT path-open-flags) error-path)
  (fd-set-add keep input)
  (fd-set-add keep output)
  (fd-set-add keep error)
  (close-file-descriptors-from 3 keep)
  (set-standard-streams input output error)
  (execve executable arguments env)
  (sc-comment "terminates the program immediately with neither scheme-level nor c-level cleanups")
  (_exit 127)
  (return SCM-UNSPECIFIED))

(define (init-sph-lib) void
  (define primitive-process-create SCM
    (scm-c-define-gsubr "primitive-process-create" 8 0 0 scm-primitive-process-create))
  (scm-set-procedure-property! primitive-process-create (scm-from-locale-symbol "documentation")
    (scm-from-locale-string
      "string (string ...) false/port/string/integer false/port/string/integer false/port/string/integer false/(string ...) (integer ...) false/integer -> false/integer\n      executable (argument ...) input output error environ-result keep-file-descriptors path-open-flags -> child-process-id\n      values for input, output or error:\n      * false: /dev/null\n      * string: filesystem path\n      * integer: file descriptor\n      * port: port\n      creates a child process via an async safe fork/exec.\n      uses execve and does not search in directories of the PATH environment variable")))