(library (sph test env)
  (export
    define-w-env-names->values
    test-env
    test-env-key
    test-env-name
    test-env-names->values
    test-env-ref
    test-env-ref*
    test-env-set!
    test-env-values->names)
  (import
    (rnrs base)
    (rnrs hashtables)
    (sph)
    (sph hashtable)
    (sph list))

  ;bindings for creating a test-environment state
  (define test-env (hashtable))
  (define (test-env-ref key) (hashtable-ref test-env key))
  (define-syntax-rule (test-env-ref* key) (test-env-ref (q key)))
  (define (test-env-set! key value) (hashtable-set! test-env key value))

  (define-syntax-rule (define-w-env-names->values (name inp exp) body ...)
    (define (name inp exp)
      ( (l (res) (if (and (boolean? res) res) exp res))
        ((l (inp exp) body ...) (test-env-names->values inp) (test-env-names->values exp)))))

  (define (test-env-names->values arg)
    "replace all names or integers with the corresponding values from the test environment"
    (if (list? arg) (map test-env-names->values arg)
      (if (or (symbol? arg) (integer? arg)) ((l (t) (if t t arg)) (test-env-ref arg)) arg))))