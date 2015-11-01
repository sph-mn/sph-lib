(library (sph error)
  (export
    error->list
    error-create
    error-create-p
    error-data
    error-exit
    error-false
    error-name
    error-origin
    error-require
    error?)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-9))

  ;error handling based on return types with an error type.

  (define-record-type error (primitive-error-create name data origin)
    error? (name error-name) (data error-data) (origin error-origin))

  (define (error-create-p . a) (apply primitive-error-create a))

  (define-syntax-rules error-create ((name data origin) (primitive-error-create name data origin))
    ((name data) (error-create name data #f)) ((name) (error-create name #f #f)))

  (define (error-false a)
    "any -> any
    result in false if \"a\" is an error, otherwise result in \"a\""
    (if (error? a) #f a))

  (define (error-exit a)
    "exit the process with status code -1 if \"a\" is an error, otherwise result in \"a\""
    (if (error? a) (begin (debug-log a) (exit -1)) a))

  (define-syntax-rules error-require
    ;requires the given assertion to be true, otherwise it results in an error
    ( (assertion when-true ...)
      (if assertion (begin when-true ...) (error-create #f (q assertion) (current-source-location))))
    ((assertion) (error-require assertion #t)))

  (define (error->list a) (list (error-origin a) (error-name a) (error-data a))))