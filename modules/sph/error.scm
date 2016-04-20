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
