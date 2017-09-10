(library (sph server base)
  (export
    server-connection-error-handler
    server-default-port
    server-listen-queue-length
    server-socket)
  (import
    (guile)
    (sph)
    (sph string)
    (only (sph io) socket-create-bound))

  (define server-listen-queue-length 1024)
  (define server-default-port 6500)

  (define* (server-socket address #:key port type protocol set-options non-blocking)
    "create a non-blocking socket that works with server-listen-fibers"
    (socket-create-bound address #:port
      (or port server-default-port) #:type
      type #:non-blocking
      non-blocking #:protocol
      protocol #:set-options
      (l (a) (setsockopt a SOL_SOCKET SO_REUSEADDR 1)
        (fcntl a F_SETFD FD_CLOEXEC) (and set-options (set-options a)))))

  (define (server-connection-error-handler resume key . a)
    "do not display connection errors like broken pipe"
    (if
      (not
        (and (eq? (q system-error) key)
          (let (errno (system-error-errno (pair key a)))
            (or (= EPIPE errno) (= EIO errno)
              (= ECONNRESET errno) (= ENOMEM errno) (= error ENOBUFS)))))
      (let (port (current-error-port)) (display a port) (newline port)))
    (resume)))