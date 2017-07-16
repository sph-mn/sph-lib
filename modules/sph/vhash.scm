(library (sph vhash)
  (export
    list->vhash
    vhash-quoted
    vhash-quoted-ref
    vhash-ref
    vhash-refq
    vhash-set
    vhash-setq)
  (import
    (ice-9 vlist)
    (sph)
    (sph list)
    (only (guile) hash)
    (only (sph alist) list->alist)
    (only (sph conditional) identity-if))

  (define-syntax-rule (vhash-quoted-ref a key) (vhash-ref a (q key)))

  (define* (vhash-ref a key #:optional default (vhash-assoc vhash-assoc))
    (let (r (vhash-assoc key a)) (if r (tail r) default)))

  (define (vhash-refq a key . default) (vhash-ref a key (first-or-false default) vhash-assq))

  (define (vhash-set-p a key value vhash-ref vhash-cons)
    (if (vhash-ref a key)
      (let loop ((rest a))
        (if (vlist-null? rest) vlist-null
          (let* ((e (vlist-head rest)) (e-key (first e)))
            (if (equal? key e-key) (vhash-cons key value (vlist-tail rest))
              (vhash-cons e-key (tail e) (loop (vlist-tail rest)))))))
      (vhash-cons key value a)))

  (define* (list->vhash a #:optional (create-hash hash)) "list [procedure] -> vhash"
    (alist->vhash (list->alist a) create-hash))

  (define-syntax-rule (vhash-quoted key/value ...) (list->vhash (quote-odd key/value ...)))
  (define (vhash-setq a key value) (vhash-set-p a key value vhash-refq vhash-consq)))
