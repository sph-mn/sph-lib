(library (sph list one)
  (export
    group
    list-ref-random
    list-ref-randomise-cycle
    list-replace-from-hashtable
    randomise)
  (import
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph list)
    (sph random-data)
    (only (rnrs base) set!))

  (define sph-list-one-description
    "additional list processing procedures which depend on libraries that depend on (sph list). to avoid circular dependencies")

  (define (list-replace-from-hashtable a ht)
    "list rnrs-hashtable -> list
     replaces elements in list that exist as key in a hashtable with the associated value.
     if the value is a list, the element is either removed (empty list) or replaced with multiple elements"
    (fold
      (l (e r)
        (let (value (ht-ref ht e)) (if value ((if (list? value) append pair) value r) (pair e r))))
      (list) a))

  (define* (group a #:optional (accessor identity))
    "procedure list -> ((any:group-key any:group-value ...):group ...)
     groups entries by unique result values of accessor.
     by default accessor is identity and groups equal elements.
     returns an association list with one entry for each group with the value as key and related values as value"
    (let loop ((rest a) (groups (alist)))
      (if (null? rest) (map (l (a) (pair (first a) (reverse (tail a)))) groups)
        (let* ((a (first rest)) (key (accessor a)) (group (alist-ref groups key)))
          (loop (tail rest) (alist-set groups key (if group (pair a group) (list a))))))))

  (define (list-ref-random a)
    "list -> any
     retrieve a random element of a list. uses the default random-state of (sph random-data) which changes with every interpreter start"
    (list-ref a (random (length a))))

  (define (list-ref-randomise-cycle a)
    "list -> procedure:{-> any}
     gives a procedure that when called gives the next element from a randomised version of \"a\"
     when the end of the list has been reached, the list is reset to a newly randomised version of \"a\""
    (let ((a-length (length a)) (new (randomise a)) (old (list)))
      (letrec
        ( (loop
            (l ()
              (if (null? new) (begin (set! new (randomise old)) (set! old (list)) (loop))
                (let (r (first new)) (set! new (tail new)) (set! old (pair r old)) r)))))
        loop)))

  (define (randomise a)
    "list -> list
     return a new list with the elements of list in random order.
     algorithm: connect a random number to each element, re-sort list corresponding to the random numbers."
    (let (length-a (length a))
      (map tail
        (list-sort (l (a b) (< (first a) (first b))) (map (l (e) (pair (random length-a) e)) a))))))
