(library (test sph module test)
  (export
    execute)
  (import
    (rnrs base)
    (sph)
    (sph alist)
    (sph test))

  (define-test (aa input output) input)
  (define (bb a b) (+ a b))

  (define-test (assertions-without-title)
    (assert-and (assert-equal (list 1 2) (list 1 2)) (assert-true (< 1 3)) (assert-true (> 1 3))))

  (define-test (assertions-with-title)
    (assert-and "a" (assert-equal "b" (list 1 2) (list 1 2))
      (assert-true "c" (< 1 3)) (assert-true "d" (> 1 3))))

  (define-tests tests-1 (aa 1 (1) (3 4) 5))
  (define-tests tests-2 (bb (1 2) 3) assertions-without-title)
  (define-tests tests-3 assertions-with-title)

  (define-test (test-execute-modules input)
    (test-execute-modules (first input)
      (alist-merge test-settings-default (ql (only (test-module))))))

  (define-tests tests
    (test-execute-modules ((test module sph)) (((test module sph test-module) "execute called")))
    (test-execute-procedures ((unquote tests-1)) (#(test-result #f "aa" 1 (3 4) (3 4) 5))
      ((unquote tests-2))
      (#(test-result #t "bb" 1 #f #f #f)
        #(test-result #f "assertions-without-title assertion" #f #f (> 1 3) #t))
      ((unquote tests-3)) (#(test-result #f "assertions-with-title a d" #f #f (> 1 3) #t)))
    (test-execute-module ((test module sph test-module)) ("execute called")))

  (define (execute) (test-result-format (test-execute-procedures tests)))
  (execute))
