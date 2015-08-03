(library (sph list one)
  (export
    list-replace-by-table
    randomise)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph hashtable)
    (sph list)
    (sph random-data)
    (only (srfi srfi-1) delete-duplicates))

  (define (randomise a)
    "list -> list
    return a new list with the elements of list in random order.
    algorithm: connect a random number to each element, re-sort list corresponding to the random numbers."
    (let (length-a (length a))
      (map tail
        (list-sort (l (a b) (< (first a) (first b))) (map (l (e) (pair (random length-a) e)) a)))))

  (define (list-replace-by-table a replacement-table . exclude)
    "(any ...) hashtable:((any:pattern replacement ...) ...) any ..."
    (delete-duplicates
      (let loop (a a)
        (fold
          (l (e r)
            (let (v (hashtable-ref replacement-table e))
              (if v
                (let ((v (any->list v)) (exclude (pair e exclude)))
                  (append v (loop (complement v exclude)) r))
                (pair e r))))
          (list) a)))))