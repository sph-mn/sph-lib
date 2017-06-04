(define-test-module (test module sph vector selection)
  (import
    (sph vector selection))

  (define-test (vector-numeric-increment-be arguments)
    (vector-numeric-increment-be (first arguments) 1))

  (test-execute-procedures-lambda
    (tuple-complexity-maximum 0 0 1 1 2 3 3 6 (3 3) 1 (2 2) 1 (2 1) 3 (3 1) 6 (3 2) 3 3 6 4 10)
    (tuple-distinct-maximum 0 0 1 1 2 4 3 27 4 256)
    (vector-numeric-increment-be #(0 0 0) #(0 0 1)
      #(0 0 1) #(0 1 0) #(0 1 0) #(0 1 1) #(0 1 1) #(1 0 0) #(1 0 0) #(1 0 1))
    (vector-complexity
      #(1 2 1 2) 7
      #(1 2 3 1 2 3) 3 + 2 + 3
      #(1 2 3 4) 10
      #(1) 1
      #(1 1) 2
      #(1 1 1) 3
      #(1 0) 3
      #(1 1 0) 5
      #(1 0 0) 5
      #(1 1 1 1) 4
      (#(1 1 0) 3 3) 1
      (#(1 1 0) 1 3) 5
      (#(1 0 1) 3 2) #f
      (#(1 1 1 1) 4) 1
      (#(1 0 1) 2 3) 3)
    (vector-selections #(1 0) (#(0 0) #(1 0) #(0 1) #(1 1))
      #(1 2 3)
      (#(3 3 3) #(2 3 3) #(1 3 3)
        #(3 2 3) #(2 2 3)
        #(1 2 3) #(3 1 3)
        #(2 1 3) #(1 1 3)
        #(3 3 2) #(2 3 2)
        #(1 3 2) #(3 2 2)
        #(2 2 2) #(1 2 2)
        #(3 1 2) #(2 1 2)
        #(1 1 2) #(3 3 1) #(2 3 1) #(1 3 1) #(3 2 1) #(2 2 1) #(1 2 1) #(3 1 1) #(2 1 1) #(1 1 1)))))
