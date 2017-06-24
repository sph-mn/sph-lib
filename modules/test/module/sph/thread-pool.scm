(define-test-module (test module sph thread-pool)
  (import
    (rnrs exceptions)
    (ice-9 threads)
    (sph one)
    (sph thread-pool))

  (define-test (thread-pool-create)
    (let (count (current-processor-count))
      (apply
        (l (queue-add! . threads)
          ;(queue-add! (nullary (raise (q test))))
          (each-integer count (l (a) (queue-add! (nullary #f)))) (thread-pool-finish threads) #t)
        (thread-pool-create count (l (key retry) key)))))

  (test-execute-procedures-lambda (thread-pool-create)))
