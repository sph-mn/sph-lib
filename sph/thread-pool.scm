(library (sph thread-pool)
  (export
    thread-pool-create
    thread-pool-destroy)
  (import
    (guile)
    (ice-9 q)
    (ice-9 threads)
    (rnrs base)
    (sph)
    (only (sph list) n-times-map))

  (define-as thread-pool-queue-lifo list
    list null?
    (l (queue a) (set! queue (pair a queue)))
    (l (queue) (let (e (first queue)) (set! queue (tail queue)) e)))

  (define thread-pool-queue-fifo (list make-q q-empty? enq! deq!))

  (define
    (thread-pool-create-thread-proc queue-mutex process-queue exception-handler exception-keys)
    (l args
      (let
        (t
          (call-with-new-thread
            (l () (lock-mutex queue-mutex)
              (if (and exception-handler exception-keys)
                (let loop ()
                  (catch exception-keys process-queue
                    (l (key . args) (apply exception-handler key loop args))))
                (process-queue)))))
        (set-thread-cleanup! t (l () (unlock-mutex queue-mutex))) t)))

  (define* (thread-pool-create #:optional count exception-handler exception-keys queue-type)
    "[integer] procedure true/symbol/(symbol ...) list/symbol:lifo/fifo -> (procedure:{procedure:{->} ->}:queue-add! thread ...)
    creates a list of threads that wait using condition variables to execute procedures from a queue, which is a first-in-first-out queue by default.
    the last-in-first-out queue just uses scheme lists and earlier tasks wait if there are not enough threads available to handle all tasks.
    the returned queue-add! procedure can be used to add procedures to the queue.
    the default thread-count is the current processor count.
    queue-type can also be a custom queue with the following signature
    queue-type: (procedure:make-queue procedure:empty?:{queue -> boolean} procedure:enqueue:{queue a ->} procedure:dequeue:{queue -> any})"
    (apply
      (l (make-queue q-empty? enq! deq!)
        (let
          ( (queue (make-queue)) (queue-mutex (make-mutex))
            (queue-not-empty (make-condition-variable)))
          (pair
            (l (a) (with-mutex queue-mutex (enq! queue a))
              (signal-condition-variable queue-not-empty))
            (n-times-map (or (and (integer? count) count) (current-processor-count))
              (letrec
                ( (process-queue
                    (l ()
                      (if (q-empty? queue) (wait-condition-variable queue-not-empty queue-mutex))
                      (or (q-empty? queue) ((deq! queue))) (process-queue))))
                (thread-pool-create-thread-proc queue-mutex process-queue
                  exception-handler exception-keys))))))
      (if (list? queue-type) queue-type
        (if (eqv? (q lifo) queue-type) thread-pool-queue-lifo thread-pool-queue-fifo))))

  (define (thread-pool-destroy thread-list) (map cancel-thread thread-list)))