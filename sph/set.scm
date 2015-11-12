;sets based on hashtables

(library (sph set)
  (export
    create-set
    create-string-set
    create-symbol-set
    set-add!
    set-add-multiple
    set-contains?
    (rename (hashtable-delete! set-delete!)))
  (import
    (rnrs base)
    (rnrs hashtables)
    (sph)
    (sph string))

  (define-syntax-rule (primitive-create-set entries hash-proc equiv)
    (let (r (make-hashtable hash-proc equiv)) (each (l (e) (hashtable-set! r e #t)) entries) r))

  (define (create-set . entries) (primitive-create-set entries equal-hash equal?))
  (define (create-string-set . entries) (primitive-create-set entries string-hash string-equal?))
  (define (create-symbol-set . entries) (primitive-create-set entries symbol-hash eq?))
  (define (set-contains? a value) (hashtable-ref a value #f))
  (define (set-add! a value) (hashtable-set! a value #t))

  (define (set-add-multiple a . entries)
    (apply create-set (append entries (vector->list (hashtable-keys a))))))
