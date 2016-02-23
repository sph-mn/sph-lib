(library (sph test base)
  (export
    test-create-result
    test-result
    test-result-arguments
    test-result-expected
    test-result-index
    test-result-result
    test-result-success?
    test-result-title
    test-result?
    test-success?)
  (import
    (rnrs base)
    (sph)
    (sph record)
    (sph vector))

  (define-record test-result type-name success? title assert-title index result arguments expected)

  (define (test-create-result . values)
    "boolean string integer any list any -> vector
    success? title index result arguments expected -> test-result"
    (apply record test-result (q test-result) values))

  (define (test-success? result expected)
    "vector/any any -> boolean
    if result is a test-result, check if it is a successful result. otherwise compare result and expected for equality"
    (if (test-result? result) (test-result-success? result) (equal? result expected)))

  (define (test-result? a) "any -> boolean"
    (and (vector? a) (= 7 (vector-length a)) (eqv? (q test-result) (vector-first a)))))
