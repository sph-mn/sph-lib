(library (sph thread-pool)
  (export
    sph-thread-pool-description
    thread-pool-create
    thread-pool-destroy
    thread-pool-finish)
  (import
    (guile)
    (ice-9 q)
    (ice-9 threads)
    (rnrs base)
    (rnrs exceptions)
    (sph)
    (only (sph list) map-integers contains?))

  (define sph-thread-pool-description
    "generic thread-pool that uses signal-conditions and has a customisable queue type")

  (define-as thread-pool-queue-lifo list
    list null?
    (l (queue a) (set! queue (pair a queue)))
    (l (queue) (let (e (first queue)) (set! queue (tail queue)) e)))

  (define thread-pool-queue-fifo (list make-q q-empty? enq! deq!))

  (define
    (thread-pool-create-thread-proc queue-mutex process-queue exception-handler exception-keys)
    "mutex procedure:nullary procedure:{any -> any} true/(symbol ...) -> procedure:{_ ... -> thread}"
    (l a
      (begin-thread
        (dynamic-wind (nullary (lock-mutex queue-mutex))
          (nullary
            (if (and exception-handler exception-keys)
              (let loop ()
                (guard
                  (obj
                    (#t
                      (if
                        (or (boolean? exception-keys)
                          (and (symbol? exception-keys) (eq? exception-keys obj))
                          (contains? exception-keys obj))
                        (exception-handler obj loop) (raise obj))))
                  (process-queue)))
              (process-queue)))
          (nullary (unlock-mutex queue-mutex))))))

  (define* (thread-pool-create #:optional count exception-handler exception-keys queue-type)
    "[integer procedure:{key retry ->} true/symbol/(symbol ...) list/symbol:lifo/fifo] -> (procedure:{procedure:nullary:code-to-execute -> unspecified}:queue-add! thread ...)
    creates a list of threads that wait using condition variables to execute procedures from a queue, which is a first-in-first-out queue by default.
    the last-in-first-out queue just uses scheme lists and earlier tasks wait if there are not enough threads available to handle all tasks.
    the returned queue-add! procedure can be used to add procedures to the queue.
    if a procedure from the queue evaluates to false, the thread it was called in exits. by filling the queue with procedures like this, all threads can be finished without having to cancel them.
    the default thread-count is the current processor count.
    the exception handler will handle rnrs exceptions and if keys to match are given then it expects symbols as the exception object.
    queue-type can be a custom queue given as a list with the following signature
    queue-type: (procedure:make-queue procedure:empty?:{queue -> boolean} procedure:enqueue:{queue a ->} procedure:dequeue:{queue -> any})"
    (apply
      (l (make-queue q-empty? enq! deq!)
        (let
          ( (queue (make-queue)) (queue-mutex (make-mutex))
            (queue-not-empty (make-condition-variable)))
          (pair
            (l (a) (with-mutex queue-mutex (enq! queue a))
              (signal-condition-variable queue-not-empty))
            (map-integers (or (and (integer? count) count) (current-processor-count))
              (letrec
                ( (process-queue
                    (nullary
                      (if (q-empty? queue) (wait-condition-variable queue-not-empty queue-mutex))
                      (if (or (q-empty? queue) ((deq! queue))) (process-queue)))))
                (thread-pool-create-thread-proc queue-mutex process-queue
                  exception-handler exception-keys))))))
      (if (list? queue-type) queue-type
        (if (eqv? (q lifo) queue-type) thread-pool-queue-lifo thread-pool-queue-fifo))))

  (define (thread-pool-destroy thread-list)
    "(thread ...) -> unspecified
    cancel all threads"
    (each cancel-thread thread-list))

  (define* (thread-pool-finish thread-list #:optional timeout)
    "(thread ...) -> (thread-exit-value ...)
    waits for all threads to finish and returns their exit values.
    this only works when all threads in the pool actually exit. but they usually wait for new entries in the queue.
    add as many procedures that return files as there are threads in the pool to exit all threads"
    (if timeout (map (l (a) (join-thread a timeout)) thread-list) (map join-thread thread-list))))
