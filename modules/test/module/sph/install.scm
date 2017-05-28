(define-test-module (test module sph install)
  (import
    (sph common)
    (sph install)
    (rnrs io ports))

  (define (process-create-with-output-to-string proc)
    "procedure{->} -> list:(integer:exit-status string)"
    (call-with-pipe
      (l (in out)
        (let (status (tail (waitpid (process-create (thunk (close in) (proc)) #f out))))
          (close out) (list status (get-string-all in))))))


  (define-test (install-cli-guile-p arguments)
    (apply (l (status output) (and (= 0 status) (string? output)))
      (process-create-with-output-to-string (thunk (apply install-cli-guile-p arguments)))))

  (test-execute-procedures-lambda (install-cli-guile-p (("--help") ()) #t (("--dry-run") ()) #t)))
