(define-test-module (test module sph lang itml write)
  (import
    (sph lang itml write)
    (sph lang itml read))

  (define-test (itml-parsed->itml arguments)
    (let* ((a (first arguments)) (result (itml-parsed->itml (string->itml-parsed a))))
      (assert-equal a result)))

  (define test-env-list-1 (q (a (b ("c" d)))))
  (define test-env-list-2 (q (a b ("c" d))))

  (test-execute-procedures-lambda
    (itml-create-indent-scm-expression ((unquote test-env-list-2)) "#a\n  b\n  \"c\"\n    d")
    (itml-create-indent-scm-expression ((unquote test-env-list-1)) "#a\n  b\n    \"c\"\n      d")
    (itml-create-inline-scm-expression ((unquote test-env-list-1)) "#(a (b (\"c\" d)))")
    (itml-create-line-scm-expression ((unquote test-env-list-1)) "#a: (b (\"c\" d))")
    (itml-parsed->itml
      ; indent-scm
      "#scm\n  (+ 1 2)\n  (+ 3 4)" #t
      ; inline-scm
      "#(scm (+ 1 2) (+ 3 4))" #t
      ; line-scm
      "#scm: (+ 1 2) (+ 3 4)" #t
      ; association
      "aa bb: cc dd" #t)))
