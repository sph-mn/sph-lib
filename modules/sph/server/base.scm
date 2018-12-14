(library (sph server base)
  (export
    server-default-port
    server-exception-handler
    server-listen-queue-length
    server-socket)
  (import
    (guile)
    (sph)
    (sph exception)
    (only (rnrs io simple) i/o-error?)
    (only (sph io) socket-create-bound))

  (define server-listen-queue-length 1024)
  (define server-default-port 6500)

  (define* (server-socket address #:key port type protocol set-options non-blocking)
    "create a socket with default options. the socket type is inferred
     from the address, which can be an ip4 or ip6 address (tcp) or a filesystem path (unix socket)"
    (socket-create-bound address #:port
      (or port server-default-port) #:type
      type #:non-blocking
      non-blocking #:protocol
      protocol #:set-options
      (l (a) (setsockopt a SOL_SOCKET SO_REUSEADDR 1)
        (fcntl a F_SETFD FD_CLOEXEC) (and set-options (set-options a)))))

  (define (server-exception-handler key . a)
    "ignore socket io errors (for example connections closed by peers).
     exceptions can be raised by rnrs raise or guile throw"
    (case key
      ( (r6rs:exception)
        (if (i/o-error? (raise-object-wrapper-obj (first a))) #f
          (exception-display-guile-r6rs key a)))
      ( (system-error)
        (let (errno (system-error-errno (pair key a)))
          (if
            (or (= EPIPE errno) (= EIO errno)
              (= ECONNRESET errno) (= ENOMEM errno) (= error ENOBUFS))
            #f (exception-display-guile key a))))
      (else (exception-display-guile key a)))))
