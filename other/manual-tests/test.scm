(import (sph common) (sph test))
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

(define-tests tests
  (test-execute ((unquote tests-1)) (#(test-result #f "aa" 1 (3 4) (3 4) 5))
    ((unquote tests-2))
    (#(test-result #t "bb" 1 #f #f #f)
      #(test-result #f "assertions-without-title assertion" #f #f (> 1 3) #t))
    ((unquote tests-3)) (#(test-result #f "assertions-with-title a d" #f #f (> 1 3) #t))))

(debug-log (every test-result-success? (test-execute tests)))
