(define-test-module (test module sph thread-pool)
  (import
    (rnrs exceptions)
    (ice-9 threads)
    (sph one)
    (sph thread-pool))

  (define-test (thread-pool-create)
    (let (size (current-processor-count))
      (apply
        (l (enqueue . threads) (= (length threads) (length (thread-pool-finish enqueue threads))))
        (thread-pool-create size (l (key retry) key)))))

  (test-execute-procedures-lambda (thread-pool-create)))
