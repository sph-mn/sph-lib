(define-test-module (test module sph record)
  (import (sph record))

  (define-record test-env-record a
    (b) (c test-env-record-cx) (d test-env-record-dx test-env-record-dx!) e)

  (define-test (define-record)
    (let (t (record test-env-record 1 2 3 4 5))
      (assert-and
        (assert-true (q default-accessors)
          (and (= 1 (test-env-record-a t)) (= 4 (test-env-record-dx t))))
        (assert-true (q setters)
          (and (begin (test-env-record-dx! t 6) (= 6 (test-env-record-dx t)))
            (begin (test-env-record-e-set! t 7) (= 7 (test-env-record-e t))))))))

  (test-execute-procedures-lambda
    define-record
    (record ((unquote test-env-record) 1 2 3) #(1 2 3 #f #f))
    (alist->record
      ((("a" . 1) ("b" . 2) ("c" 3 4)) (unquote (make-record-layout (list "c" "a" "b")))) #((3 4) 1 2))
    (record-field-names (unquote test-env-record) #(a b c d e))))
