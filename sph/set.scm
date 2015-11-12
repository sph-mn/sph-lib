;sets based on hashtables

(library (sph set)
  (export
    set-create
    set-string-create
    set-symbol-create
    set-add!
    set-add-multiple
    set-contains?
    (rename (hashtable-delete! set-delete!)))
  (import
    (rnrs base)
    (rnrs hashtables)
    (sph)
    (sph string))

  (define-syntax-rule (primitive-set-create entries hash-proc equiv)
    (let (r (make-hashtable hash-proc equiv)) (each (l (e) (hashtable-set! r e #t)) entries) r))

  (define (set-create . entries) (primitive-set-create entries equal-hash equal?))
  (define (set-string-create . entries) (primitive-set-create entries string-hash string-equal?))
  (define (set-symbol-create . entries) (primitive-set-create entries symbol-hash eq?))
  (define (set-contains? a value) (hashtable-ref a value #f))
  (define (set-add! a value) (hashtable-set! a value #t))

  (define (set-add-multiple a . entries)
    (apply set-create (append entries (vector->list (hashtable-keys a))))))
