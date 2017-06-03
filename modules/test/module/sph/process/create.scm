(define-test-module (test module sph process create)
  (import
    (sph process)
    (sph process create)
    (sph read-write)
    (sph list)
    (sph one))

  (define-test (process-create)
    (call-with-pipes 1
      (l (in out)
        (let (pid (process-create "echo" (list "test") #f out #:search-path? #t)) (close out)
          (assert-and (integer? pid) (equal? "test\n" (port->string in))
            (process-finish-success? pid))))))

  (define (close-multiple . a) "calls close for all arguments" (each close a))

  (define-test (process-chain)
    (call-with-pipes 2
      (l (in-1 out-1 in-2 out-2)
        (let
          (pids
            (process-chain in-1 out-2
              (list "cat" "cat" (list "tr" "e" "f") "cat") #:search-path? #t))
          (close-multiple in-1 out-2)
          (assert-and (not (null? pids)) (every integer? pids)
            (begin (display "test" out-1) (force-output out-1)
              (close out-1) (assert-equal "tfst" (port->string in-2)))
            (every process-finish-success? pids))))))

  (test-execute-procedures-lambda (process-create) (process-chain)))
