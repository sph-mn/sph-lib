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
    (rename (hashtable-delete! set-delete!)))
  (import
    (rnrs base)
    (rnrs hashtables)
    (sph)
    (sph string))

  (define sph-set-description "hashtables as sets")

  (define-syntax-rule (primitive-set-create set entries)
    (let (r set) (each (l (a) (hashtable-set! r a #t)) entries) r))

  (define (set-add-multiple! a entries) (each (l (b) (hashtable-set! a b #t)) entries))
  (define (set-create-empty initial-size) (make-hashtable equal-hash equal? initial-size))

  (define (set-create-string-empty initial-size)
    (make-hashtable string-hash string-equal? initial-size))

  (define (set-create-symbol-empty initial-size) (make-hashtable symbol-hash eq? initial-size))
  (define (set-create . entries) (primitive-set-create (set-create-empty (length entries)) entries))

  (define (set-create-string . entries)
    (primitive-set-create (set-create-string-empty (length entries)) entries))

  (define (set-create-symbol . entries)
    (primitive-set-create (set-create-symbol-empty (length entries)) entries))

  (define (set-contains? a value) (hashtable-contains? a value))
  (define (set-add! a value) (hashtable-set! a value #t))

  (define (set-add-multiple a . entries)
    (apply set-create (append entries (vector->list (hashtable-keys a))))))
