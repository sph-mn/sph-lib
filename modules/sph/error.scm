(library (sph error)
  (export
    error->list
    error-create
    error-create-p
    error-create-record
    error-data
    error-group
    error-id
    error-if-exception
    error-let*
    error-pass
    error-return
    error?)
  (import
    (guile)
    (rnrs base)
    (sph)
    (srfi srfi-9))

  ;an error type

  (define-record-type error (error-create-record id group data)
    error? (id error-id) (group error-group) (data error-data))

  (define (error-create-p id group data) (error-create-record id group data))

  (define-syntax-rules error-create
    ;"[id group data] -> error
    ;create an error with optional field values. fields that are not set are set to false"
    ((id group data) (error-create-record id group data)) ((id group) (error-create id group (list)))
    ((id) (error-create id #f (list))))

  (define-syntax-rule (error-if-exception body ...)
    ;converts exceptions to errors.
    ;"evaluate body like in a \"begin\" but if an exception occurs, result in an error with
    ;the error-id being the exception key, error-group being the current module name and error-data being the exception arguments".
    (catch #t (thunk body ...)
      (l (key . a)
        (error-create key (module-name (current-module))
          (if (null? a) a (list (pair (q exception-data) a)))))))

  (define-syntax-cases error-let* s
    ;"like let* but results in an error when a binding value evaluates to an error. similar to and-let*"
    ( ( ( (identifier value) ...) body ...)
      (let loop (bindings (syntax->datum (syntax ((identifier value) ...))))
        (if (null? bindings) (syntax (begin body ...))
          (let (binding (first bindings))
            (with-syntax
              ( (body (loop (tail bindings))) (identifier (datum->syntax s (first binding)))
                (value (datum->syntax s (first (tail binding)))))
              (syntax ((lambda (identifier) (if (error? identifier) identifier body)) value)))))))
    ( ( (identifier value) body ...)
      (syntax ((lambda (identifier) (if (error? identifier) identifier (begin body ...))) value))))

  (define (error->list a) "error -> (any:id any:group any:data)"
    (list (error-id a) (error-group a) (error-data a)))

  (define-syntax-rules error-pass
    ;"if \"a\" is an error, call \"consequent\" with \"a\", otherwise call \"alternative\" with a"
    ((a consequent alternative) (let (b a) (if (error? b) (consequent b) (alternative b))))
    ((a consequent) (error-pass a consequent identity)))

  (define-syntax-rules error-return
    ((a consequent alternative) (error-pass a alternative consequent))
    ((a consequent) (error-pass a identity consequent))))
