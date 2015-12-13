(library (sph conditional)
  (export
    apply-pass-if
    boolean-and
    check
    check-not
    false-if
    false-if-not
    identity-if
    pass-and
    pass-if)
  (import
    (rnrs base)
    (sph))

  (define (apply-pass-if a proc)
    "list procedure:{any ... -> any} -> any
    like pass-if but uses apply to use the contents of \"a\" as arguments to proc"
    (if a (apply proc a) #f))

 (define-syntax-rules pass-if
    ;"any procedure:{any -> any} -> any
    ;apply proc with "a" if "a" is a true value, otherwise return false or evaluate else"
    ((a proc) ((lambda (b) (if b (proc b) #f)) a))
    ((a proc else) ((lambda (b) (if b (proc b) else)) a)))

  (define (pass-and proc . a)
    "procedure:{any ... -> any} any ... -> any
    apply proc with \"a\" as multiple arguments if all given \"a\" are true values"
    (if (any not a) #f (apply proc a)))

  (define-syntax-rule (identity-if result-if-true else ...)
    ;result in "test" if "test" is true, otherwise execute "else"
    ((lambda (r) (if r r (begin else ...))) result-if-true))

  (define-syntax-rule (false-if test consequent)
    ;result in false if "test" is true, otherwise execute consequent
    (if test #f consequent))

  (define-syntax-rule (false-if-not test consequent)
    ;execute consequent if "test" is true, otherwise result in false
    (if test consequent #f))

  (define-syntax-rules check
    ;the result of "expr" is passed to all "proc" separately. if all are true, the result of expr is the result, otherwise false
    (((proc ...) expr) ((lambda (r) (and (proc r) ...) r #f) expr))
    ((proc expr) ((lambda (r) (if (proc r) r #f)) expr)))

  (define-syntax-rules check-not
    ;the result of "expr" is passed to all "proc" separately. if none are true, the result of expr is the result, otherwise false
    (((proc ...) expr) ((lambda (r) (if (or (proc r) ...) #f r)) expr))
    ((proc expr) ((lambda (r) (if (proc r) #f r)) expr)))

  (define-syntax-rules boolean-and
    ;like "and" but returns false if any value is not a boolean
    ((expr) ((l (r) (if (eqv? #t r) #t r)) expr))
    ((expr rest ...) ((l (r) (if (eq? #t r) (boolean-and rest ...) r)) expr))))