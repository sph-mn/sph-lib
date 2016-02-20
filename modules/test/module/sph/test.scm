(define-test-module (test module sph test)
  (import
    (sph alist)
    (sph test))

  (define-test (aa input output) input)
  (define-test (bb a b) (+ a b))

  (define-test (assertions-without-title)
    (assert-and (assert-equal (list 1 2) (list 1 2)) (assert-true (< 1 3)) (assert-true (> 1 3))))

  (define-test (assertions-with-title)
    (assert-and "a" (assert-equal "b" (list 1 2) (list 1 2))
      (assert-true "c" (< 1 3)) (assert-true "d" (> 1 3))))

  (define-procedure-tests tests-1 (aa 1 (1)))
  (define-procedure-tests tests-2 (bb (1 2) 3) assertions-without-title)
  (define-procedure-tests tests-3 assertions-with-title)

  (define-test (test-execute-modules-prefix input)
    (test-execute-modules-prefix (first input)
      (alist-merge test-settings-default (ql (m-only (test-module))))))

  (define-test (test-execute-procedures arguments)
    (test-execute-procedures test-settings-default arguments))

  (test-execute-procedures-lambda
    ;(test-modules-prefix-execute ((test module sph)) (((test module sph test-module) "execute called")))   ;
    (test-execute-procedures (unquote tests-1) (#(test-result #t "aa" #f 1 #f #f #f)))
    (test-execute-module ((unquote test-settings-default) (test module sph test-module))
      ("execute called"))))
