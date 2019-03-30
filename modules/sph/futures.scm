(library (sph futures)
  (export
    future
    make-future
    sph-futures-description
    sph-futures-pool
    touch)
  (import
    (sph)
    (sph thread-pool)
    (only (guile) usleep)
    (only (rnrs base) set!))

  (define-syntax-rule (future exp) (make-future (nullary exp)))

  (define sph-futures-description
    "fine-grain parallelism based on (sph thread-pool).
     example usage: (touch (future (+ 2 3)))")

  (define sph-futures-pool #f)
  (define pool-enqueue)

  (define (make-future a)
    "procedure:{-> any} -> future
     return a future that evaluates the given procedure possibly in another thread.
     use touch to get the result"
    ; the pool can not be created when the module is created because call-with-new-thread would block.
    (if (not sph-futures-pool)
      (begin (set! sph-futures-pool (thread-pool-create))
        (set! pool-enqueue (first sph-futures-pool))))
    (let (result (vector (q future) #f #f))
      (pool-enqueue (nullary (vector-set! result 2 (a)) (vector-set! result 1 #t))) result))

  (define (touch a)
    "future -> any
     wait until the future is finished and return its result"
    ; maybe something other than sleep can be used
    (if (vector-ref a 1) (vector-ref a 2) (begin (usleep 500000) (touch a)))))
