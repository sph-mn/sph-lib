(define-module (sph server))

(use-modules (sph) (sph exception)
  (sph module) (sph server base)
  (sph string) (sph thread-pool) ((ice-9 threads) #:select (current-processor-count)))

(re-export server-socket server-default-port)
(export server-listen sph-server-description)

(define sph-server-description
  "a generic socket data processing server that uses a thread-pool for parallel request processing.
   starting the server with server-listen makes it listen on an existing or newly created socket.
   if there is a new connection on the socket, a user supplied procedure is called with a client port to receive and send data")

(define with-signal-handling
  (let (signal-numbers (list SIGPIPE SIGINT SIGTERM))
    (l (s c)
      "socket procedure -> any
       setup sigint and sigterm signals to break the blocking accept call.
       accept is interrupted when the socket is closed and subsequently results in an EBADF error.
       reset signal handlers to the original ones on any kind of exit"
      (let ((handlers #f) (stop (l (n) (close-port s))))
        (dynamic-wind
          (nullary (set! handlers (map sigaction signal-numbers (list SIG_IGN stop stop)))) c
          (nullary
            (each (l (n handler) (sigaction n (first handler) (tail handler))) signal-numbers
              handlers)))))))

(define (with-thread-pool size c)
  "call c with a new thread pool and pass it an enqueue procedure.
   if size is equal or lower than 1, no thread pool is created but the passed procedures
   work as if. the thread pool is exited waiting for the last requests to finish on any kind of exit"
  (if (<= size 1) (c (l (a) (a)))
    (apply
      (l (enqueue . threads) (exception-always (thread-pool-finish enqueue threads) (c enqueue)))
      (thread-pool-create size))))

(define (with-accept-error-handling thunk) "return if accept is interrupted"
  (catch (q system-error) thunk
    (l a
      (if (and (= EBADF (system-error-errno a)) (string-equal? "accept" (first (tail a)))) #t
        (thunk)))))

(define* (server-listen handle-request socket #:key parallelism (exception-key #t))
  "procedure:{port:client -> unspecified} port:socket [#:key parallelism integer/false] -> unspecified
   listen for new connections on socket and call handle-request with a input/output port for the client.
   handle-request is called in the next free thread in a thread-pool.
   the server is stopped when it receives the signal SIGINT or SIGTERM.
   by default all exceptions are catched, printed and the server continues listening.
   this can be changed with giving a different exception-key, which is passed to catch,
   for example one that will never be matched"
  (listen socket server-listen-queue-length)
  (with-thread-pool
    (if parallelism (if (boolean? parallelism) (max 1 (- (current-processor-count) 1)) parallelism)
      1)
    (l (enqueue)
      (with-signal-handling socket
        (nullary
          (let loop-accept ()
            (with-accept-error-handling
              (nullary
                (let (conn (accept socket))
                  (if conn
                    (let (client (first conn))
                      (enqueue
                        (nullary
                          (catch exception-key
                            (nullary (handle-request client) (force-output client))
                            server-exception-handler)
                          (catch #t (close-port client) (l a #t))))))
                  (loop-accept))))))))))
