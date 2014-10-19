(library (sph list one)
  (export
    randomise)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph random-data))

  (define (randomise lis)
    "list -> list
    return a new list with the elements of lis in random order.
    algorithm: connect a random number to each element, re-sort list corresponding to the random numbers."
    (let (length-lis (length lis))
      (map tail
        (list-sort (l (a b) (< (first a) (first b)))
          (map (l (ele) (cons (random-integer length-lis) ele)) lis))))))