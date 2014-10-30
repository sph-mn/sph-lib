;error handling based on return types and an error type

(library (sph error)
  (export
    error-create
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

  (define-record-type error (error-create origin name data)
    error? (origin error-origin) (name error-name) (data error-data))

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
      (if assertion (begin when-true ...) (error-create (current-source-location) #f (q assertion))))
    ((assertion) (error-require assertion #t))))