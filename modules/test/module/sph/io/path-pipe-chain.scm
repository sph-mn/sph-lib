(define-test-module (test module sph io path-pipe-chain)
  (import
    (sph io)
    (rnrs io ports)
    (sph list)
    (sph one)
    (ice-9 threads)
    (sph io path-pipe-chain))

  (define (join-threads a)
    ;(each join-thread a)
    (each (l (a) (and (thread? a) (join-thread a))) a))

  (define message "test")

  (define-test (path-pipe-chain-ends) "test conversion of first-input and last-output"
    (assert-true "nothing-nothing"
      (first-or-false
        (path-pipe-chain #f #f (vector (q nothing) (q nothing) (l (in out) (not (or in out)))))))
    (assert-equal "nothing-nothing-port-path" message
      (let (out (tmpnam))
        (path-pipe-chain #f out
          (vector (q nothing) (q port) (l (in out) (display message out) (close out))))
        (file->string out)))
    (assert-equal "nothing-nothing-path-port" message
      (call-with-output-string
        (l (out)
          (path-pipe-chain #f out
            (vector (q nothing) (q path)
              (l (in out) (call-with-output-file out (l (out) (display message out)))))))))
    (assert-equal "port-path-nothing-nothing" message
      (call-with-output-string
        (l (out)
          (path-pipe-chain (open-input-string message) out
            (vector (q path) (q port)
              (l (in out) (call-with-input-file in (l (in) (port-copy-all in out)))))))))
    (assert-equal "path-port-nothing-nothing" message
      (call-with-output-string
        (l (out)
          (let (in (tmpnam)) (string->file message in)
            (path-pipe-chain in out
              (vector (q port) (q port) (l (in out) (port-copy-all in out) (close in)))))))))

  (define (path-pipe-chain-links-call . path-pipe-chain-config)
    (call-with-output-string
      (l (out) (join-threads (apply path-pipe-chain #f out path-pipe-chain-config)))))

  (define-test (path-pipe-chain-links)
    (assert-equal "nothing-port-path-nothing" message
      (path-pipe-chain-links-call
        (vector (q nothing) (q port)
          (l (in out)
            (begin-thread (let (out (open (out) O_WRONLY)) (display message out) (close out)))))
        (vector (q path) (q port)
          (l (in out) (begin-thread (call-with-input-file in (l (in) (port-copy-all in out))))))))
    (assert-equal "nothing-port-port-nothing" message
      (path-pipe-chain-links-call
        (vector (q nothing) (q port) (l (in out) (begin-thread (display message out) (close out))))
        (vector (q port) (q port) (l (in out) (begin-thread (port-copy-all in out) (close in))))))
    (assert-equal "nothing-path-port-nothing" message
      (path-pipe-chain-links-call
        (vector (q nothing) (q path) (l (in out) (begin-thread (string->file message out))))
        (vector (q port) (q port)
          (l (in out)
            (begin-thread (let (in (open (in) O_RDONLY)) (port-copy-all in out) (close in)))))))
    (assert-equal "nothing-path-path-nothing" message
      (path-pipe-chain-links-call
        (vector (q nothing) (q path) (l (in out) (begin-thread (string->file message out))))
        (vector (q path) (q port) (l (in out) (begin-thread (file->port in out)))))))

  (test-execute-procedures-lambda (path-pipe-chain-ends) (path-pipe-chain-links)))
