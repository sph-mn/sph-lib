;an error type

(library (sph error)
  (export
    error->list
    error-create
    error-create-p
    error-data
    error-exit
    error-false
    error-guard
    error-guard-call
    error-if
    error-if-call
    error-name
    error-origin
    error-require
    error?)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-9))

  (define-record-type error (primitive-error-create name data origin)
    error? (name error-name) (data error-data) (origin error-origin))

  (define (error-create-p . a)
    "[name data origin] -> error
    create an error with optional field values. fields that are not set are set to false"
    (apply primitive-error-create a))

  (define-syntax-rules error-create ((name data origin) (primitive-error-create name data origin))
    ((name data) (error-create name data #f)) ((name) (error-create name #f #f)))

  (define-syntax-rules error-if
    ;"evaluate \"consequent\" if \"a\" is an error object, otherwise evaluate \"alternative\".
    ;if no \"alternative\" expression is given, give \"a\". \"a\" is evaluated only once"
    ((a consequent alternative) (if (error? a) consequent alternative))
    ((a consequent) (let (b a) (error-if b consequent b))))

  (define-syntax-rules error-if-call
    ;"call \"proc-consequent\" with \"a\" if \"a\" is an error object, otherwise call \"proc-alternative\" with \"a\".
    ;if no \"proc-alternative\" is given, give \"a\""
    ( (a proc-consequent proc-alternative)
      (let (b a) ((if (error? b) proc-consequent proc-alternative) b)))
    ((a proc-consequent) (let (b a) (if (error? b) (proc-consequent b) b))))

  (define (error-if-call a proc) (if (error? a) (proc a) a))
  (define-syntax-rule (error-guard a else ...) (let (b a) (if (error? b) b (begin else ...))))
  (define (error-guard-call a proc) (if (error? a) a (proc a)))

  (define (error-false a)
    "any -> any
    result in false if \"a\" is an error, otherwise result in \"a\""
    (if (error? a) #f a))

  (define (error-exit a)
    "exit the process with status code -1 if \"a\" is an error, otherwise result in \"a\""
    (if (error? a) (begin (debug-log a) (exit -1)) a))

  (define-syntax-rules error-require
    ;requires the given assertion expression to evaluate to true, otherwise it results in an error
    ( (assertion when-true ...)
      (if assertion (begin when-true ...) (error-create #f (q assertion) (current-source-location))))
    ((assertion) (error-require assertion #t)))

  (define (error->list a) "error -> (any:name any:data any:origin)"
    (list (error-name a) (error-data a) (error-origin a))))
