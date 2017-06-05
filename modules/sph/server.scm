; a generic socket-based server that uses a thread-pool for request processing

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
    (rnrs base)
    (rnrs bytevectors)
    (sph)
    (sph thread-pool)
    (only (sph filesystem) ensure-directory-structure)
    (only (sph list) contains?)
    (only (sph string) string-equal?))

  (define sph-server-description
    "a generic socket based server
    uses a thread-pool for parallel request processing and can use as many cpu cores as there are available. the thread-pool pattern is not a bad design")

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
    - if address is a path starting with \"/\" then a local unix socket is created (no port necessary)
    - if address contains \":\" then an ip6 tcp socket is created
    - else an ip4 tcp socket is created"
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
      (dynamic-wind (nullary (set! handlers (map sigaction signal-numbers (list SIG_IGN stop stop))))
        proc
        (nullary
          (map (l (n handler) (sigaction n (first handler) (tail handler))) signal-numbers handlers)))))

  (define (call-with-exception-handling exception-keys exception-handler loop-listen socket proc)
    "boolean/(symbol ...) false/procedure:{key procedure:resume exception-arguments ... -> any} procedure:resume procedure -> any
    if exception-handler or -keys is not false then install given these handlers for the inner request processing.
    the exception-handler receives a procedure to resume listening"
    (if (and exception-handler exception-keys)
      (catch exception-keys proc (l (key . a) (apply exception-handler key loop-listen socket a)))
      (proc)))

  (define (call-with-epipe-and-ebadf-handling loop-listen proc)
    "procedure:continue-listening procedure:nullary -> any
    handle broken pipe errors and the accept error when the socket is closed"
    (catch (q system-error) proc
      (l exc
        (let (errno (system-error-errno exc))
          (if (= EPIPE errno) (loop-listen)
            (if (and (= EBADF errno) (string-equal? "accept" (list-ref exc 1))) #f
              (apply throw exc)))))))

  (define-syntax-rule
    (loop-listen exception-keys exception-handler socket connection-identifier body ...)
    (call-with-signal-handling socket
      (nullary
        (let loop-listen ()
          (call-with-exception-handling exception-keys exception-handler
            loop-listen socket
            (nullary
              (call-with-epipe-and-ebadf-handling loop-listen
                (nullary
                  (let loop-process (connection-identifier (first (accept socket)))
                    (setvbuf connection-identifier _IONBF 0) body
                    ... (loop-process (first (accept socket))))))))))))

  (define (thread-pool-handle-sigpipe exception-handler exception-keys)
    (l (key resume . args)
      (if (and (eqv? (q system-error) key) (= EPIPE (system-error-errno (pair key args)))) (resume)
        (if
          (and exception-keys exception-handler
            (or (boolean? exception-keys) (and (symbol? exception-keys) (eqv? key exception-keys))
              (and (list? exception-keys) (contains? exception-keys key))))
          (apply exception-handler key resume args) (apply throw key args)))))

  (define* (server-listen proc socket #:optional worker-count exception-handler exception-keys)
    "procedure:{port ->} socket procedure:{resume key a ... ->} symbol/(symbol ...)/boolean-true ->
    worker-count is the number of separate processing threads. 1 means no separate threads and single-thread operation.
    the default is equal to the current processor count or 1 if less than 3 processors are available.
    with only 2 processors (cores) the overhead of using multiple workers could likely diminish performance (it did so in tests).
    the default exception handler catches all exceptions and resumes"
    (listen socket server-listen-queue-length)
    (let (cpu-count (current-processor-count))
      (if (if worker-count (= 1 worker-count) (< cpu-count 3))
        (loop-listen exception-keys exception-handler socket c (proc c) (close-port c))
        (apply
          (l (queue-add! . thread-pool)
            (loop-listen exception-keys exception-handler
              socket c (queue-add! (nullary (proc c) (close-port c))))
            (thread-pool-destroy thread-pool))
          (thread-pool-create (or worker-count cpu-count)
            (thread-pool-handle-sigpipe exception-handler exception-keys) #t))))))
