(library (sph conditional)
  (export
    boolean-true?
    false-if
    false-if-not
    identity-if
    if-pass
    if-pass-apply
    if-predicate-and
    if-predicate-or
    pass-predicate-and-if
    pass-predicate-or-if
    predicate-and
    predicate-or)
  (import
    (rnrs base)
    (sph))

  (define-syntax-rule (any->list a) (if (list? a) a (list a)))

  (define-syntax-rule (identity-if result-if-true else ...)
    ;result in "test" if "test" is true, otherwise execute "else"
    ((lambda (r) (if r r (begin else ...))) result-if-true))

  (define-syntax-rule (false-if test consequent)
    ;result in false if "test" is true, otherwise execute consequent
    (if test #f consequent))

  (define-syntax-rule (false-if-not test consequent)
    ;execute consequent if "test" is true, otherwise result in false
    (if test consequent #f))

  (define (pass-predicate-and-if predicates subject consequent alternative)
    "(procedure ...) any procedure:{any:subject -> any} procedure:{any:subject -> any}"
    (if (every (l (a) (a subject)) (any->list predicates)) (consequent subject)
      (alternative subject)))

  (define (pass-predicate-or-if predicates subject consequent alternative)
    "any/(procedure:{any:subject -> any} ...) any procedure:{any:subject -> any} procedure:{any:subject -> any}"
    (if (any (l (a) (a subject)) (any->list predicates)) (consequent subject) (consequent subject)))

  (define (predicate-and predicates . subjects)
    "any/(procedure:{any:subject -> any} ...) any ... -> boolean
    true if every predicate gives true for every subject, false otherwise"
    (every (l (a) (every (l (b) (a b)) subjects)) (any->list predicates)))

  (define (predicate-or predicates . subjects)
    "any/(procedure:{any:subject -> any} ...) any ... -> boolean
    true if every predicate gives true for every subject, false otherwise"
    (any (l (a) (any (l (b) (a b)) subjects)) (any->list predicates)))

  (define-syntax-rules if-predicate-and
    ( ( (predicate ...) subject consequent alternative)
      (let (b subject) (if (and (predicate b) ...) consequent alternative)))
    ( (predicate subject consequent alternative)
      (if-predicate-and (predicate) subject consequent alternative)))

  (define-syntax-rules if-predicate-or
    ( ( (predicate ...) subject consequent alternative)
      (let (b subject) (if (or (predicate b) ...) consequent alternative)))
    ( (predicate subject consequent alternative)
      (if-predicate-or (predicate) subject consequent alternative)))

  (define-syntax-rules if-pass
    ;"any procedure:{any -> any} -> any
    ;call proc with "a" if "a" is a true value, otherwise return false or evaluate else.
    ;also known as \"and=>\""
    ((a consequent alternative) (let (b a) (if b (consequent b) alternative)))
    ((a proc) (if-pass a proc #f)))

  (define (if-pass-apply a consequent alternative)
    "list procedure:{any ... -> any} -> any
    like if-pass but uses apply to use the contents of \"a\", which should be a list in the true case, as arguments to proc"
    (if a (apply consequent a) alternative))

  (define-syntax-rules boolean-true? ((a ...) (and (equal? #t a) ...))))
