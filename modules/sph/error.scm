(library (sph error)
  (export
    error->list
    error-create
    error-create-p
    error-data
    error-false-if
    error-identity-if
    error-log-and-exit
    error-name
    error-origin
    error-pass-if
    error-pass-if-not
    error-pass-if-p
    error-when-not
    error?)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-9))

  ;an error type

  (define-record-type error (error-create-p name data origin)
    error? (name error-name) (data error-data) (origin error-origin))

  (define-syntax-rules error-create
    ;"[name data origin] -> error
    ;create an error with optional field values. fields that are not set are set to false"
    ((name data origin) (error-create-p name data origin))
    ((name data) (error-create-p name data #f)) ((name) (error-create-p name #f #f)))

  (define (error-pass-if-p a consequent alternative)
    "any procedure:{any -> any} procedure:{any -> any} -> any
    if \"a\" is an error, call \"consequent\" with \"a\", otherwise call \"alternative\" with a"
    (if (error? a) (consequent a) (alternative a)))

  (define-syntax-rules error-pass-if
    ((a consequent alternative) (error-pass-if-p a consequent alternative))
    ((a consequent) (error-pass-if-p a consequent identity)))

  (define-syntax-rules error-pass-if-not
    ((a consequent alternative) (error-pass-if-p a alternative consequent))
    ((a consequent) (error-pass-if-p a identity consequent)))

  (define-syntax-rule (error-identity-if a else ...)
    ;"if \"a\" is an error, give \"a\", otherwise evaluate "else""
    (let (b a) (if (error? b) b (begin else ...))))

  (define (error-false-if a)
    "any -> any
    if \"a\" is an error, give false, otherwise \"a\""
    (if (error? a) #f a))

  (define* (error-log-and-exit a #:optional (exit-code -1))
    "error integer ->
    write a representation of error to standard output and exit with \"exit-code\", which is -1 by default"
    (debug-log a) (exit exit-code))

  (define-syntax-rule (error-when-not test consequent ...)
    (if test (begin consequent ...) (error-create #f (q error-when-not) (current-source-location))))

  (define (error->list a) "error -> (any:name any:data any:origin)"
    (list (error-name a) (error-data a) (error-origin a))))
