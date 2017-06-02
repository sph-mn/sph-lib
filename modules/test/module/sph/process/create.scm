(define-test-module (test module sph process create)
  (import
    (sph process create)
    (sph read-write)
    (sph one))

  (define (call-with-pipes pair-count c)
    (call-multiple-times)
    )

  (define-test (process-create)
    (let (ports (pipe))
      (integer?
        (process-create "/usr/bin/echo"
          (list "test") (tail ports)))))

  (define-test (process-chain)
    (let ((input-ports (pipe)) (output-ports (pipe)))
      (assert-true
        (not
          (null?
            (map waitpid
              (begin-first
                (process-chain (first input-ports) (tail output-ports)
                  (list "cat" "cat" (list "tr" "es" "fg") "cat" (list "wc" "-c")) #:search-path? #t)
                (display "test" (tail input-ports)) (debug-log (port->string (first output-ports))))))))))

  (test-execute-procedures-lambda (process-create) (process-chain)))
