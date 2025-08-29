(define-module (sph server fibers))
(use-modules (fibers) (sph) (sph server base) ((ice-9 threads) #:select (current-processor-count)))
(export server-listen-fibers server-socket-fibers sph-server-fibers-description)

(define sph-server-fibers-description
  "a generic socket data processing server that uses fibers for parallel request processing and non-blocking port input/output.
   fibers are cooperative, event-driven user threads. there can be multiple active fibers per kernel thread.
   see https://github.com/wingo/fibers/wiki/Manual for details and caveats.
   starting the server with server-listen makes it listen on an existing or newly created socket.
   if there is a new connection on the socket, a user supplied procedure is called with a client port to receive and send data")

(define* (server-socket-fibers address #:key port type protocol set-options)
  "create a non-blocking socket that works with server-listen-fibers"
  (server-socket address #:port
    port #:type type #:protocol protocol #:set-options set-options #:non-blocking #t))

(define* (server-listen-fibers handle-request socket #:key parallelism)
  "procedure:{port:client -> unspecified} port:socket [#:parallelism integer/false]
   starts a server with a new fibers scheduler.
   the server is stopped when it receives the signal SIGINT or SIGTERM.
   currently all exceptions are catched automatically by (fibers) and
   printed and the server continues listening"
  (run-fibers
    (nullary (listen socket server-listen-queue-length) (sigaction SIGPIPE SIG_IGN)
      (let loop-accept ()
        (let (conn (accept socket (logior SOCK_NONBLOCK SOCK_CLOEXEC)))
          (if conn
            (let (client (first conn))
              (spawn-fiber (nullary (handle-request client)) #:parallel? #t)))
          (loop-accept))))
    #:parallelism (or parallelism (current-processor-count))))
