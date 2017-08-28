(library (sph server)
  (export
    server-address-string->protocol-family
    server-create-bound-socket
    server-listen
    server-listen-queue-length
    server-protocol-family->address-family
    server-receive-buffer-size
    server-send-buffer-size
    sph-server-description)
  (import
    (guile)
    (ice-9 threads)
    (rnrs bytevectors)
    (rnrs exceptions)
    (sph)
    (sph thread-pool)
    (only (sph filesystem) ensure-directory-structure)
    (only (sph list) contains?)
    (only (sph string) string-equal?))

  (define sph-server-description
    "a generic socket based server
     uses a thread-pool for parallel request processing and can use as many cpu cores as there are available.
     basic use case: starting the server makes it listen on an existing or newly created socket. if there is a connection,
     a custom user procedure is called with the connection object and a port to receive and send data")

  (define server-listen-queue-length 1024)
  (define server-send-buffer-size 8096)
  (define server-receive-buffer-size 512)

  (define (server-address-string->protocol-family a) "string -> integer"
    (if (string-prefix? "/" a) PF_UNIX (if (string-index a #\:) PF_INET6 PF_INET)))

  (define (server-protocol-family->address-family a) "integer -> integer"
    (if (= PF_UNIX a) AF_UNIX (if (= PF_INET6 a) AF_INET6 AF_INET)))

  (define*
    (server-create-bound-socket address #:optional (port-number 6500) (type SOCK_STREAM)
      (protocol 0))
    "string [integer integer integer] -> socket
     create a socket, bind, and result in the socket object.
     defaults:
     * if address is a path starting with \"/\" then a local unix socket is created (no port necessary)
     * if address contains \":\" then an ip6 tcp socket is created
     * else an ip4 tcp socket is created"
    (let (protocol-family (server-address-string->protocol-family address))
      (let
        ( (r (socket protocol-family type protocol))
          (address-family (server-protocol-family->address-family protocol-family)))
        (setsockopt r SOL_SOCKET SO_REUSEADDR 1)
        (setsockopt r SOL_SOCKET SO_SNDBUF server-send-buffer-size)
        (setsockopt r SOL_SOCKET SO_RCVBUF server-receive-buffer-size)
        (if (= address-family AF_UNIX)
          (begin
            (if (file-exists? address) (delete-file address)
              (ensure-directory-structure (dirname address)))
            (bind r address-family address))
          (bind r address-family (inet-pton address-family address) port-number))
        r)))

  (define (call-with-signal-handling s proc)
    "socket procedure -> any
     setup sigint and sigterm signals for stopping the listening, and reset to original handlers on any kind of exit"
    (let
      ((signal-numbers (list SIGPIPE SIGINT SIGTERM)) (handlers #f) (stop (l (n) (close-port s))))
      (dynamic-wind
        (nullary (set! handlers (map sigaction signal-numbers (list SIG_IGN stop stop)))) proc
        (nullary
          (map (l (n handler) (sigaction n (first handler) (tail handler))) signal-numbers handlers)))))

  (define (call-with-exception-handling exception-handler loop-listen socket thunk)
    "boolean/(symbol ...) false/procedure:{key procedure:resume exception-arguments ... -> any} procedure:resume procedure -> any
     if exception-handler or -keys is not false then install given these handlers for the inner request processing.
     the exception-handler receives a procedure to resume listening"
    (if exception-handler (guard (obj (#t (exception-handler obj loop-listen socket))) (thunk))
      (thunk)))

  (define (call-with-epipe-and-ebadf-handling loop-listen proc)
    "procedure:continue-listening procedure:nullary -> any
     handle broken pipe errors and the accept error when the socket is closed"
    (catch (q system-error) proc
      (l a
        (let (errno (system-error-errno a))
          (if (= EPIPE errno) (loop-listen)
            (if (and (= EBADF errno) (string-equal? "accept" (list-ref a 1))) #f (raise a)))))))

  (define (server-listen-loop exception-handler socket on-connection)
    (call-with-signal-handling socket
      (nullary
        (let loop-listen ()
          (call-with-exception-handling exception-handler loop-listen
            socket
            (nullary
              (call-with-epipe-and-ebadf-handling loop-listen
                (nullary
                  (let loop-accept (conn (first (accept socket)))
                    (setvbuf conn (q none) 0) (on-connection conn)
                    (loop-accept (first (accept socket))))))))))))

  (define*
    (server-listen proc socket #:optional thread-count exception-handler #:key dispatcher-thread?)
    "procedure:{port ->} socket procedure:{resume key a ... ->} symbol/(symbol ...)/boolean-true ->
     thread-count is the total number of threads to use for processing including the request dispatcher.
     the default is equal to the current processor count.
     the default exception handler catches all exceptions and resumes"
    ; no connections are accepted as long as server-listen-queue-length is full
    (let (thread-count (or thread-count (current-processor-count)))
      (listen socket server-listen-queue-length)
      (if (<= thread-count 1)
        (server-listen-loop exception-handler socket (l (conn) (proc conn) (close-port conn)))
        (apply
          (l (enqueue . threads)
            (server-listen-loop exception-handler socket
              (l (conn) (enqueue (nullary (proc conn) (close-port conn)))))
            (thread-pool-destroy threads))
          (thread-pool-create (if dispatcher-thread? (- thread-count 1) thread-count)))))))
