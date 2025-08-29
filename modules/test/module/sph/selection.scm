(define-test-module (test module sph selection)
  (import
    (sph selection)
    (srfi srfi-41))

  (define-test (vector-numeric-increment-be arguments)
    (vector-numeric-increment-be (first arguments) 1))

  (define-test (vector-distinct-stream arguments)
    (stream->list (apply vector-distinct-stream arguments)))

  (test-execute-procedures-lambda
    (vector-distinct-maximum 0 0 1 1 2 3 3 6 (3 3) 1 (2 2) 1 (2 1) 3 (3 1) 6 (3 2) 3 3 6 4 10)
    (vector-selection-maximum 0 0 1 1 2 4 3 27 4 256)
    (vector-numeric-increment-be #(0 0 0) #(0 0 1)
      #(0 0 1) #(0 1 0) #(0 1 0) #(0 1 1) #(0 1 1) #(1 0 0) #(1 0 0) #(1 0 1))
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
        #(1 1 2) #(3 3 1) #(2 3 1) #(1 3 1) #(3 2 1) #(2 2 1) #(1 2 1) #(3 1 1) #(2 1 1) #(1 1 1)))
    (vector-distinct-count #(1 2 1 2) 7
      #(1 2 3 1 2 3) 15
      #(1 2 3 4) 10
      #(1) 1
      #(1 1) 2
      #(1 1 1) 3
      #(1 0) 3
      #(1 1 0) 5
      #(1 0 0) 5
      #(1 1 1 1) 4
      (#(1 1 0) 3 3) 1 (#(1 1 0) 1 3) 5 (#(1 0 1) 3 2) 0 (#(1 1 1 1) 4) 1 (#(1 0 1) 2 3) 3)
    (vector-distinct-stream (#(1 2 1 2)) (#(1 2 1 2) #(1 2 1) #(2 1 2) #(1 2) #(2 1) 1 2))))
