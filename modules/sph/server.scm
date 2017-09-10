(library (sph server)
  (export
    server-default-port
    server-listen
    server-socket
    sph-server-description)
  (import
    (guile)
    (rnrs bytevectors)
    (sph)
    (sph exception)
    (sph server base)
    (sph string)
    (sph thread-pool)
    (only (ice-9 threads) current-processor-count))

  (define sph-server-description
    "a generic socket data processing server that uses a thread-pool for parallel request processing.
     starting the server with server-listen makes it listen on an existing or newly created socket.
     if there is a new connection on the socket, a user supplied procedure is called a client port to receive and send data")

  (define with-signal-handling
    (let (signal-numbers (list SIGPIPE SIGINT SIGTERM))
      (l (s c)
        "socket procedure -> any
        setup sigint and sigterm signals to break the blocking accept call.
        reset signal handlers to the original ones on any kind of exit"
        (let ((handlers #f) (stop (l (n) (close-port s))))
          (dynamic-wind
            (nullary (set! handlers (map sigaction signal-numbers (list SIG_IGN stop stop)))) c
            (nullary
              (each (l (n handler) (sigaction n (first handler) (tail handler))) signal-numbers
                handlers)))))))

  (define (with-thread-pool size c)
    (if (<= size 1) (c (l (a) (a)))
      (apply (l (enqueue . threads) (exception-always (thread-pool-destroy threads) (c enqueue)))
        (thread-pool-create size server-connection-error-handler))))

  (define (with-accept-error-handling thunk) "continue unless socket has been closed"
    (catch (q system-error) thunk
      (l a
        (let (errno (system-error-errno a))
          (if (and (= EBADF errno) (string-equal? "accept" (first (tail a)))) #t (thunk))))))

  (define* (server-listen handle-request socket #:key parallelism)
    "server is stopped with when it receives the signal SIGINT or SIGTERM"
    (listen socket server-listen-queue-length)
    (with-thread-pool (max 1 (- (or parallelism (current-processor-count)) 1))
      (l (enqueue)
        (with-signal-handling socket
          (nullary
            (let loop-accept ()
              (with-accept-error-handling
                (nullary
                  (let (conn (accept socket))
                    (if conn
                      (enqueue
                        (nullary
                          (let (client (first conn)) (handle-request client)
                            (force-output client) (close-port client)))))
                    (loop-accept)))))))))))
