(library (sph set)
  (export
    set-add!
    set-add-multiple
    set-contains?
    set-create
    set-create-empty
    set-create-string
    set-create-string-empty
    set-create-symbol
    set-create-symbol-empty
    sph-set-description
    (rename (ht-delete! set-delete!)))
  (import
    (sph hashtable)
    (sph)
    (sph string))

  (define sph-set-description "hashtables as sets")

  (define-syntax-rule (primitive-set-create set entries)
    (let (r set) (each (l (a) (ht-set! r a #t)) entries) r))

  (define (set-add-multiple! a entries) (each (l (b) (ht-set! a b #t)) entries))
  (define (set-create-empty initial-size) (ht-make ht-hash-equal equal? initial-size))

  (define (set-create-string-empty initial-size)
    (ht-make ht-hash-string string-equal? initial-size))

  (define (set-create-symbol-empty initial-size) (ht-make ht-hash-symbol eq? initial-size))
  (define (set-create . entries) (primitive-set-create (set-create-empty (length entries)) entries))

  (define (set-create-string . entries)
    (primitive-set-create (set-create-string-empty (length entries)) entries))

  (define (set-create-symbol . entries)
    (primitive-set-create (set-create-symbol-empty (length entries)) entries))

  (define (set-contains? a value) (ht-contains? a value))
  (define (set-add! a value) (ht-set! a value #t))

  (define (set-add-multiple a . entries)
    (apply set-create (append entries (vector->list (ht-keys a))))))
