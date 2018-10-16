(library (sph exception)
  (export
    exception-always
    exception-display-guile
    exception-display-guile-r6rs
    exception-intercept
    exception-intercept-if
    exception-printer
    raise-object-wrapper-obj
    raise-object-wrapper?
    sph-exception-description)
  (import
    (ice-9 match)
    (rnrs exceptions)
    (rnrs io ports)
    (rnrs io simple)
    (sph)
    (only (sph module) import-unexported))

  (define sph-exception-description "rnrs exception helpers. experimental")

  (define-syntax-rule (begin-first-values a b ...)
    ; like begin but returns the multiple values created by the first expression
    (apply-values (l result b ... (apply values result)) a))

  (define-syntax-rule (exception-intercept on-exception expression)
    ; evaluate "on-exception" if an exception occurred in expression
    (guard (a (#t on-exception (raise a))) expression))

  (define-syntax-rule (exception-intercept-if expression on-exception no-exception)
    ; evaluate "on-exception" if an exception occurred in expression.
    ; evaluate "no-exception" if no exception occurred and return the result of expression
    (begin-first-values (exception-intercept on-exception expression) no-exception))

  (define-syntax-rule (exception-always always expression)
    ; evaluate nullary "always" after expression even if an exception occurred
    (let (a (nullary always)) (exception-intercept-if expression (a) (a))))

  (import-unexported (rnrs exceptions) exception-printer)
  (import-unexported (rnrs records procedural) raise-object-wrapper-obj)
  (import-unexported (rnrs records procedural) raise-object-wrapper?)

  (define (exception-display-guile-r6rs key a)
    "display information about an r6rs exception object received by guile catch"
    (let (port (current-error-port))
      (exception-printer port key
        a
        (nullary
          (match a
            ( ( (? raise-object-wrapper? wrapper))
              (put-datum port (raise-object-wrapper-obj wrapper)))
            (else (put-datum port (q r6rs-exception))))))
      (put-char port #\newline)))

  (define (exception-display-guile key a)
    (let (port (current-error-port)) (put-string port "guile-exception: ")
      (put-datum port key) (put-char port #\space) (put-datum port a) (put-char port #\newline))))
