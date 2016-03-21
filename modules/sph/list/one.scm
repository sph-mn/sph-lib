(library (sph list one)
  (export
    group
    group-with-accessor
    list-ref-cycle-randomise-proc
    list-ref-random
    list-replace-by-table
    randomise)
  (import
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph list)
    (sph random-data)
    (only (guile) identity)
    (only (rnrs hashtables) hashtable-set!)
    (only (srfi srfi-1) delete-duplicates))

  ; additional list processing procedures which depend on libraries that depend on (sph list) to avoid a circular dependency

  (define (group a) "list -> ((any:group-key any:group-value ...):group ...)"
    (group-with-accessor identity a))

  (define (group-with-accessor accessor a)
    "procedure list -> ((any:group-key any:group-value ...):group ...)"
    (let loop ((rest a) (groups (alist)))
      (if (null? rest) groups
        (let* ((e (first rest)) (key (accessor e)) (group (alist-ref groups key)))
          (loop (tail rest) (alist-set groups key (if group (pair e group) (list e))))))))

  (define (list-ref-random a)
    "list -> any
    retrieve a random element of a list. uses the default random-state of (sph random-data) which changes with every interpreter start"
    (list-ref a (random (length a))))

  (define (list-ref-cycle-randomise-proc a)
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
