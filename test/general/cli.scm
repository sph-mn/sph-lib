(import (sph) (sph cli) (sph test))

(define test-env-cli-config
  (list #:name "test-interface"
    #:options
    (ql (input (#\i "input") #t #t #f integer "this option can be used for specifying input files")
      (test #\t) ((output-file input-file ...) #f #t))
    #:version (ql 0 1)
    #:help "test help" #:about "-my-custom-about-text-" #:missing-arguments-handler #f))

(define test-env-cli (apply cli-create test-env-cli-config))
(define (test-general inp) (test-env-cli (list "a" "b" "c" "-i" "1")))

(define (test-required)
  (assert-and
    (assert-true "named"
      (catch (q missing-arguments) (thunk (test-env-cli))
        (l (key count missing) (equal? (ql input) missing)))))
  (assert-true "unnamed"
    (catch (q missing-arguments) (thunk (test-env-cli (list "-i" "1")))
      (l (key count missing) (eqv? 1 count)))))

(execute-tests-quasiquote (general #t ((output-file . "a") (input-file "b" "c") (input . 1)))
  (required))
