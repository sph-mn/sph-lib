(pre-include "libguile.h" "dirent.h")

(pre-define (scm-c-define-procedure-c name required optional rest c-function documentation)
  "defines and registers a c routine as a scheme procedure with documentation.
  like scm-c-define-gsubr but also sets documentation.
  scm-c-define-procedure-c-init must have been called in scope"
  (set scm-c-define-procedure-c-temp (scm-c-define-gsubr name required optional rest c-function))
  (scm-set-procedure-property! scm-c-define-procedure-c-temp
    (scm-from-locale-symbol "documentation") (scm-from-locale-string documentation)))

(pre-define-if-not-defined open-max 1024)

(define (close-file-descriptors-from start-fd) (void int)
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
    (pre-if have-sysconf (set maxfd (sysconf _SC-OPEN-MAX)) (set maxfd (getdtablesize))))
  (if (< maxfd 0) (set maxfd open-max)) (set fd start-fd)
  (while (< fd maxfd) (convert-type (close (convert-type fd int)) void) (set fd (+ 1 fd))))

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

(define
  (scm-process-create scm-executable scm-arguments scm-input-port scm-output-port scm-error-port
    scm-env
    scm-keep-descriptors)
  (SCM SCM SCM SCM SCM SCM SCM SCM) (define process-id int (fork))
  (if (not (= 0 process-id)) (return (scm-from-int process-id))) (close-file-descriptors-from 4 )
  (define input int (port-argument->fd scm-input-port O_RDONLY))
  (define output int (port-argument->fd scm-output-port O_WRONLY))
  (define error int (port-argument->fd scm-error-port O_WRONLY))
  (if (> input 0)
    (begin (if (= 0 output) (move-fd output)) (if (= 0 error) (move-fd error)) (dupf2-fd input)))
  (if (> output 1) (begin (if (= 1 error) (move-fd error)) (dupf2-fd output)))
  (if (> error 2) (dupf2-fd error)) (execve executable env arguments)
  ; terminates the program immediately, with neither scheme-level nor c-level cleanups
  (_exit 127) (return SCM-UNSPECIFIED))

(define (init-sph-lib) void (scm-c-define-procedure-c "process-create" 1 0 0 scm-process-create ""))
(define (scm-init-sph-lib) void (scm-c-define-module "sph process create" init-sph-lib 0))
