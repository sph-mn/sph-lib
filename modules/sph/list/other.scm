(define-module (sph list other))
(use-modules (srfi srfi-1) (rnrs sorting) (sph) (sph alist) (sph hashtable) (sph list))

(export group group-recursively
  list-ref-random list-ref-randomise-cycle
  list-replace-from-hashtable randomise sph-list-other-description)

(define sph-list-other-description
  "additional list processing bindings that depend on libraries that depend on (sph list). to avoid circular dependencies")

(define (list-replace-from-hashtable a ht)
  "list rnrs-hashtable -> list
   replaces elements in list that exist as key in a hashtable with the associated value.
   if the value is a list, the element is either removed (empty list) or replaced with multiple elements"
  (fold
    (l (e r)
      (let (value (ht-ref ht e)) (if value ((if (list? value) append pair) value r) (pair e r))))
    (list) a))

(define* (group a #:optional (accessor identity))
  "list [procedure:{any -> any}] -> ((any:group-key any:group-value ...):group ...)
   groups entries by unique result values of accessor.
   by default accessor is identity and groups equal elements.
   returns an association list with one entry for each group with the value as key and related values as value"
  (let loop ((rest a) (groups (alist)))
    (if (null? rest) (map (l (a) (pair (first a) (reverse (tail a)))) groups)
      (let* ((a (first rest)) (key (accessor a)) (group (alist-ref groups key)))
        (loop (tail rest) (alist-set groups key (if group (pair a group) (list a))))))))

(define* (group-recursively a #:optional (accessor first))
  "((any ...) ...) [procedure] -> list
   group lists and the elements of groups until no further sub-groups are possible.
   the default accessor is \"first\".
   # example
       (group-recursively (list (list 1 2 3) (list 1 2 6) (list 1 4 7) (list 8 9)) first)
       -> ((1 (2 3 6) (4 7)) (8 9))
   note in the example input how the entries after 1 2 have been grouped into (2 3 6)
   # example use case
   converting a list of filesystem paths split at slashes to a nested list where prefixes are directories"
  (map
    (l (a) "(group-name element ...)"
      (let (rest (map tail (remove (compose null? tail) (tail a))))
        (if (null? rest) (first a) (pair (first a) (group-recursively rest)))))
    (group a accessor)))

(define* (list-ref-random a #:optional (random-state *random-state*))
  "list -> any
   retrieve a random element of a list"
  (list-ref a (random (length a) random-state)))

(define* (list-ref-randomise-cycle a #:optional (random-state *random-state*))
  "list -> procedure:{-> any}
   gives a procedure that when called gives the next element from a randomised version of \"a\"
   when the end of the list has been reached, the list is reset to a newly randomised version of \"a\""
  (let ((a-length (length a)) (new (randomise a random-state)) (old (list)))
    (letrec
      ( (loop
          (l ()
            (if (null? new)
              (begin (set! new (randomise old random-state)) (set! old (list)) (loop))
              (let (r (first new)) (set! new (tail new)) (set! old (pair r old)) r)))))
      loop)))

(define* (randomise a #:optional (random-state *random-state*))
  "list -> list
   return a new list with the elements of list in random order.
   algorithm: connect a random number to each element, re-sort list corresponding to the random numbers."
  (let (length-a (length a))
    (map tail
      (list-sort (l (a b) (< (first a) (first b)))
        (map (l (c) (pair (random length-a random-state) c)) a)))))
