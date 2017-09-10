(library (sph server fibers)
  (export
    server-listen-fibers
    server-socket-fibers
    sph-server-fibers-description)
  (import
    (fibers)
    (guile)
    (sph)
    (sph server base)
    (only (ice-9 threads) current-processor-count))

  (define sph-server-fibers-description
    "a generic socket data processing server that uses fibers for parallel request processing and non-blocking port input/output.
     fibers are cooperative, event-driven user threads. there can be multiple active fibers per kernel thread.
     starting the server with server-listen makes it listen on an existing or newly created socket.
     if there is a new connection on the socket, a user supplied procedure is called a client port to receive and send data")

  (define* (server-socket-fibers address #:key port type protocol set-options)
    "create a non-blocking socket that works with server-listen-fibers"
    (server-socket address #:port
      port #:type type #:protocol protocol #:set-options set-options #:non-blocking #t))

  (define* (server-listen-fibers handle-request socket #:key parallelism)
    "procedure socket [#:parallelism integer/false]
     starts a server with a new fibers scheduler"
    (run-fibers
      (nullary (listen socket server-listen-queue-length) (sigaction SIGPIPE SIG_IGN)
        (let loop-accept ()
          (let (conn (accept socket (logior SOCK_NONBLOCK SOCK_CLOEXEC)))
            (if conn
              (let (client (first conn))
                (spawn-fiber (nullary (handle-request client)) #:parallel? #t)))
            (loop-accept))))
      #:parallelism (or parallelism (current-processor-count)))))
