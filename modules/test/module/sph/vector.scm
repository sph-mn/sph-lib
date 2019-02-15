(define-test-module (test module sph vector)
  (import
    (sph vector))

  (test-execute-procedures-lambda (vector-append (#(1 2 3) #(4 5 6)) #(1 2 3 4 5 6))
    (vector-range (#(1 2 3 4 5 6) 1 4) #(2 3 4 5)
      (#(1 2 3 4 5 6) 1) #(2 3 4 5 6) (#(1 2 3 4 5 6) 0 5) #(1 2 3 4 5 6))
    (vector-select (#(1 2 3 4 5 6) #(1 3 4)) #(2 4 5))
    (vector-relative-change-index/value (#(0 0 20 0 3 0) #(0 0 20 0 3 0)) 0
      (#(0 0 20 0 3 0) #(0 20 0 0 3 0)) 1/8 (#(0 3) #(3 0)) 1/2)))
