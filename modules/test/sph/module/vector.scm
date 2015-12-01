(import
  (sph)
  (sph vector)
  (sph test))

(execute-tests-quasiquote
  (vector-append
    (#(1 2 3) #(4 5 6))
    #(1 2 3 4 5 6))
  (vector-produce
    ((unquote *) #(2 3) #(5 6 7))
    #(#(10 12 14) #(15 18 21)))
  (vector-range-ref
    (#(1 2 3 4 5 6) 1 4)
    #(2 3 4 5)
    (#(1 2 3 4 5 6) 1)
    #(2 3 4 5 6)
    (#(1 2 3 4 5 6))
    #(1 2 3 4 5 6))
  (vector-select
    (#(1 2 3 4 5 6) #(1 3 4))
    #(2 4 5)))