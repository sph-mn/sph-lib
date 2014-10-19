;manage bindings in a hashtable, this can be part of an evaluation environment

(library (sph lang docl env)
  (export
    docl-env-bindings
    docl-env-ref
    docl-env-reset!
    docl-env-set!
    docl-env-update!)
  (import
    (sph)
    (only (guile)
      make-fluid
      fluid-ref
      fluid-set!)
    (only (rnrs base) define)
    (except (rnrs hashtables) hashtable-ref)
    (only (sph hashtable)
      hashtable
      hashtable-ref
      hashtable-merge!))

  (define docl-env-bindings (make-fluid))
  (define (docl-env-reset!) (fluid-set! docl-env-bindings (hashtable)))
  (define (docl-env-ref key) (hashtable-ref (fluid-ref docl-env-bindings) key))
  (define (docl-env-set! key value) (hashtable-set! (fluid-ref docl-env-bindings) key value))

  (define (docl-env-update! bindings.new)
    (hashtable-merge! (fluid-ref docl-env-bindings) bindings.new))

  (docl-env-reset!))