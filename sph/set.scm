(library (sph set)
  ;sets based on hashtables
  (export
    (rename (hashtable-delete! set-delete!))
    set-contains?
    set-add-multiple
    set-add!
    create-set
    create-string-set
    create-symbol-set)
  (import
    (sph string)
    (rnrs base)
    (rnrs hashtables)
    (sph))

  (define-syntax-rule (primitive-create-set entries hash-proc equiv)
    (let (res (make-hashtable hash-proc equiv))
      (each (l (ele) (hashtable-set! res ele #t)) entries)
      res))

  (define (create-set . entries) (primitive-create-set entries equal-hash equal?))
  (define (create-string-set . entries) (primitive-create-set entries string-hash string-equal?))
  (define (create-symbol-set . entries) (primitive-create-set entries symbol-hash eq?))
  (define (set-contains? arg value) (hashtable-ref arg value #f))
  (define (set-add! arg value) (hashtable-set! arg value #t))

  (define (set-add-multiple arg . entries)
    (apply create-set (append entries (vector->list (hashtable-keys arg))))))
