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
    (rnrs io ports)
    (sph)
    (only (rnrs base) set!)
    (only (sph list) map-integers)
    (only (sph one) each-integer))

  (define sph-thread-pool-description
    "thread-pool that uses wait-conditions to pause unused threads and has a customisable queue type")

  (define-as thread-pool-queue-lifo list
    list null?
    (l (queue a) (set! queue (pair a queue)))
    (l (queue) (let (e (first queue)) (set! queue (tail queue)) e)))

  (define thread-pool-queue-fifo (list make-q q-empty? enq! deq!))

  (define* (thread-pool-create #:optional size exception-handler queue-type)
    "[integer procedure:{key retry ->} true/symbol/(symbol ...) list/symbol:lifo/fifo] -> (procedure:{procedure:nullary:code-to-execute -> unspecified}:enqueue! thread ...)
     creates a list of threads that wait using condition variables to execute procedures from a queue, which is a first-in-first-out queue by default.
     the last-in-first-out queue just uses scheme lists and earlier tasks wait if there are not enough threads available to handle all tasks.
     the returned enqueue! procedure can be used to add procedures to the queue.
     if a procedure from the queue evaluates to false, the thread it was called in exits. by filling the queue with procedures like this,
     all threads can be closed without having to cancel them by other more obscure means.
     the default size is the current processor count.
     queue-type can be a custom queue given as a list with the following signature
     queue-type: (procedure:make-queue procedure:empty?:{queue -> boolean} procedure:enqueue:{queue a ->} procedure:dequeue:{queue -> any})"
    (apply
      (l (make-queue q-empty? enq! deq!)
        (let
          ( (queue (make-queue)) (queue-mutex (make-mutex))
            (queue-not-empty (make-condition-variable))
            (size (or (and (integer? size) size) (current-processor-count))))
          (letrec*
            ( (get-task
                (nullary
                  (or
                    (with-mutex queue-mutex
                      (if (q-empty? queue) (wait-condition-variable queue-not-empty queue-mutex)
                        (deq! queue))
                      (if (q-empty? queue) #f (deq! queue)))
                    (get-task))))
              (process-queue (nullary (if ((get-task)) (process-queue) #f))))
            (let*
              ( (enqueue
                  (l (a) (with-mutex queue-mutex (enq! queue a))
                    (signal-condition-variable queue-not-empty)))
                (exception-handler
                  (if exception-handler (l a (apply exception-handler process-queue a))
                    (l a
                      (let (port (current-error-port)) (put-datum port a) (put-char port #\newline))
                      (process-queue)))))
              (pair enqueue
                (map-integers size (l (n) (call-with-new-thread process-queue exception-handler))))))))
      (if (list? queue-type) queue-type
        (if (eqv? (q lifo) queue-type) thread-pool-queue-lifo thread-pool-queue-fifo))))

  (define (thread-finish) "a procedure returning false instructs a thread in the pool to end" #f)

  (define* (thread-pool-finish enqueue thread-list #:optional timeout)
    "(thread ...) -> (thread-exit-value ...)
    let threads complete all currently enqueued tasks and exit"
    ; since a thread finishes after dequeing one finish procedure,
    ; number-of-threads procedures should be enough to finish all threads.
    (each-integer (length thread-list) (l (n) (enqueue thread-finish)))
    (if timeout (map (l (a) (join-thread a timeout)) thread-list)
      (map join-thread thread-list))))
