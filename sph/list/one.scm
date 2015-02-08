(library (sph list one)
  (export
    randomise)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph random-data))

  (define (randomise a)
    "list -> list
    return a new list with the elements of list in random order.
    algorithm: connect a random number to each element, re-sort list corresponding to the random numbers."
    (let (length-a (length a))
      (map tail
        (list-sort (l (a b) (< (first a) (first b)))
          (map (l (e) (pair (random length-a) e)) a))))))