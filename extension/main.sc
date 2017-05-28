(pre-define (debug-log format ...)
  (fprintf stderr (pre-string-concat "%s:%d " format "\n") __func__ __LINE__ __VA_ARGS__))

(pre-define imht-set-key-t int)
(pre-define imht-set-can-contain-zero? 0)
(sc-include "foreign/imht-set")
(pre-include "libguile.h" "dirent.h" "unistd.h" "limits.h")
(pre-define-if-not-defined OPEN-MAX 1024)

(pre-define (scm-c-define-procedure-c name required optional rest c-function documentation)
  "defines and registers a c routine as a scheme procedure with documentation.
  like scm-c-define-gsubr but also sets documentation.
  scm-c-define-procedure-c-init must have been called in scope"
  (set scm-c-define-procedure-c-temp (scm-c-define-gsubr name required optional rest c-function))
  (scm-set-procedure-property! scm-c-define-procedure-c-temp
    (scm-from-locale-symbol "documentation") (scm-from-locale-string documentation)))

(define (close-file-descriptors-from start-fd keep) (void int imht-set-t*)
  "try to close all used file descriptors greater than start-fd" (define fd long maxfd long)
  (define-array path-proc-fd char (PATH_MAX))
  (define directory DIR* path-length int entry (struct dirent*) first-invalid char*)
  (set path-length
    (snprintf path-proc-fd, (sizeof path-proc-fd) "/proc/%ld/fd" (convert-type (getpid) long)))
  (if
    (and (> path-length 0) (<= (convert-type path-length size-t) (sizeof path-proc-fd))
      (set directory (opendir path-proc-fd)))
    (begin
      (while (not (= 0 (set entry (readdir directory))))
        (set fd (strtol (struct-pointer-get entry d-name) (address-of first-invalid) 10))
        (if
          (and (not (= (struct-pointer-get entry d-name) first-invalid))
            (= (deref first-invalid) #\nul) (>= fd 0)
            (< fd INT_MAX) (>= fd start-fd) (not (= fd (dirfd directory))))
          (convert-type (close (convert-type fd int)) void)))
      (convert-type (closedir directory) void))
    (begin (pre-if HAVE-SYSCONF (set maxfd (sysconf _SC-OPEN-MAX)) (set maxfd (getdtablesize)))
      (if (< maxfd 0) (set maxfd OPEN-MAX)) (set fd start-fd)
      (while (< fd maxfd)
        (if (not (imht-set-contains? keep fd)) (convert-type (close (convert-type fd int)) void))
        (set fd (+ 1 fd))))))

(pre-define (port-argument->fd a open-flags)
  "SCM integer -> integer
  if \"a\" is false false then it returns a file descriptor for /dev/null.
  if \"a\" is a guile port, returns its file descriptor.
  if \"a\" is an integer, converts it to c.
  otherwise the result is -1"
  (if* (= SCM-BOOL-F a) (open "/dev/null" open-flags)
    (if* (scm-is-true (scm-port? a)) (scm->int (scm-fileno a))
      (if* (scm-is-integer a) (scm->int a) -1))))

(pre-define (move-fd a) (do-while (= errno EINTR) (set a (dup a))))
(pre-define (dup2-fd old new) (do-while (= errno EINTR) (dup2 old new)) (close old))
(pre-define (close-fd scm a) (if (= scm SCM-BOOL-F) (close a)))

(pre-define (create-keep-set scm-keep-descriptors keep) (define keep imht-set-t* status 0)
  (if (imht-set-create (+ 3 (scm->uint32 (scm-length scm-keep-descriptors))) (address-of keep))
    (return SCM-BOOL-F))
  (while (not (scm-is-null scm-keep-descriptors))
    (if (imht-set-add keep (scm->int (SCM-CAR scm-keep-descriptors)))
      (begin (imht-set-destroy keep) (return SCM-BOOL-F)))
    (set scm-keep-descriptors (SCM-CDR scm-keep-descriptors))))

(pre-define (set-standard-streams input-port output-port error-port)
  (if (> input 0)
    (begin (if (= 0 output) (move-fd output)) (if (= 0 error) (move-fd error)) (dup2-fd input)))
  (if (> output 1) (begin (if (= 1 error) (move-fd error)) (dup2-fd output)))
  (if (> error 2) (dup2-fd error)))

(define (scm-string-list->string-pointer-array scm-a) (SCM char**)
  (define a-length int (scm->int (scm-length scm-a)))
  (define result char** (malloc (* (sizeof char*) (+ 1 a-length)))) (set (deref result a-length) 0)
  (define result-pointer char** result)
  (while (not (scm-is-null a)) (define b char* b-length size-t)
    (set b (scm->locale-stringn (SCM-CAR scm-a) (address-of b-length)))
    (set (deref result-pointer) b) (set (deref result-pointer b-length) 0)
    (set scm-a (SCM-CDR scm-a)))
  (return result))

(define
  (scm-primitive-process-create scm-executable scm-arguments scm-input-port scm-output-port
    scm-error-port
    scm-env
    scm-keep-descriptors)
  (SCM SCM SCM SCM SCM SCM SCM SCM)
  (define arguments char** (scm-string-list->string-pointer-array arguments))
  (define env char**
    (if (= SCM-BOOL-F scm-env) environ (scm-string-list->string-pointer-array scm-env)))
  (define executable char* (scm->locale-string scm-executable))
  (create-keep-set scm-keep-descriptors keep)
  ;(define process-id int (fork))
  #;(if (not (= 0 process-id))
  (begin
  (free arguments) (free executable)
      (if (not (= SCM-BOOL-F scm-env)) (free env)) (imht-set-destroy keep)
      (return (scm-from-int process-id))))
  ;after fork
  (define input int (port-argument->fd scm-input-port O_RDONLY))
  (define output int (port-argument->fd scm-output-port O_WRONLY))
  (define error int (port-argument->fd scm-error-port O_WRONLY)) (imht-set-add keep input)
  (imht-set-add keep output) (imht-set-add keep error)
  ;(close-file-descriptors-from 0 keep)
  ;(set-standard-streams scm-input-port scm-output-port scm-error-port) (debug-log "%s" executable)
  ;(execve executable env arguments)
  ; terminates the program immediately, with neither scheme-level nor c-level cleanups
  ;(_exit 127)
  (return SCM-UNSPECIFIED))

(define (init-sph-lib) void
  (scm-c-define-gsubr "primitive-process-create" 7 0 0 scm-primitive-process-create))
