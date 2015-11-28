(library (sph vhash)
  (export
        vhash-ref
    vhash-refq
    vhash-set
    vhash-setq
    vhash-quoted
)
  (import     (ice-9 vlist)
)
(define (vhash-ref a key) (identity-if (vhash-assoc key a) #f))
  (define (vhash-refq a key) (identity-if (vhash-assq key a) #f))

  (define (vhash-set-p a key value vhash-ref vhash-cons)
    (if (vhash-ref a key)
      (let loop ((rest a))
        (if (vlist-null? rest) vlist-null
          (let* ((e (vlist-head rest)) (e-key (first e)))
            (if (equal? key e-key) (vhash-cons key value (vlist-tail rest))
              (vhash-cons e-key (tail e) (loop (vlist-tail rest)))))))
      (vhash-cons key value a)))

  (define (vhash-setq a key value) (vhash-set-p a key value vhash-refq vhash-consq))
  )
