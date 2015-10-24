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
    like pass-if but uses apply to use the contents of a as arguments to proc"
    (if a (apply proc a) #f))

  (define-syntax-rules pass-if
    ;"any procedure:{any -> any} -> any
    ;apply proc with a if a is a true value"
    ((a proc) ((lambda (b) (if b (proc b) #f)) a))
    ((a proc else) ((lambda (b) (if b (proc b) else)) a)))

  (define (pass-and proc . a)
    "procedure:{any ... -> any} any ... -> any
    apply proc with a as multiple arguments if all given a are true values"
    (if (any not a) #f (apply proc a)))

  (define-syntax-rule (identity-if result-if-true else ...)
    ;result in test if test is true, otherwise execute else
    ((lambda (r) (if r r (begin else ...))) result-if-true))

  (define-syntax-rule (false-if test consequent)
    ;result in false if test is true, otherwise execute consequent
    (if test #f consequent))

  (define-syntax-rule (false-if-not test consequent)
    ;execute consequent if test is true, otherwise result in false
    (if test consequent #f))

  (define-syntax-rules check
    ;return the result of executing expr only after all given proc applied with the result
    ;resulted in a true value
    (((proc ...) expr) ((lambda (res) (and (proc res) ...) res #f) expr))
    ((proc expr) ((lambda (res) (if (proc res) res #f)) expr)))

  (define-syntax-rules check-not
    ;return the result of executing expr only after none of the given proc applied with the result
    ;resulted in a true value
    (((proc ...) expr) ((lambda (res) (if (or (proc res) ...) #f res)) expr))
    ((proc expr) ((lambda (res) (if (proc res) #f res)) expr)))

  (define-syntax-rules boolean-and
    ;like "and" but returns false if any value is not a boolean
    ( (expr)
      ((l (res) (if (eqv? #t res) #t res)) expr))
    ((expr rest ...) ((l (res) (if (eq? #t res) (boolean-and rest ...) res)) expr))))