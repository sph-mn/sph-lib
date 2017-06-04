(define-test-module (test module sph install)
  (import
    (sph io)
    (sph process)
    (sph install))

  (define-test (install-cli-guile-p in)
    (let ((cli-arguments (first in)) (install-spec (tail in)))
      (assert-true
        (< 0
          (string-length
            (process-eval
              (qq
                ( (import (sph) (sph install))
                  (apply install-cli-guile-p (q (unquote cli-arguments)) (q (unquote install-spec)))))
              port->string))))))

  (test-execute-procedures-lambda (install-cli-guile-p (("--help") ()) #t)))
