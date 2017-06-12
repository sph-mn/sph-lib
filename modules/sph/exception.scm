(library (sph exception)
  (export
    exception-always
    exception-intercept
    exception-intercept-if
    sph-exception-description)
  (import
    (rnrs exceptions)
    (only (sph one) begin-first)
    (sph)))

(define sph-exception-description "rnrs exception helpers. experimental")

(define-syntax-rule (exception-intercept on-exception expression)
  ; evaluate "on-exception" if an exception occurred in expression
  (guard (a (#t on-exception (raise a))) expression))

(define-syntax-rule (exception-intercept-if expression on-exception no-exception)
  ; evaluate "on-exception" if an exception occurred in expression.
  ; evaluate "no-exception" if no exception occurred and return the result of expression
  (begin-first (exception-intercept on-exception expression) no-exception))

(define-syntax-rule (exception-always always expression)
  ; evaluate nullary "always" after expression even if an exception occurred
  (let (a (nullary always)) (exception-intercept-if expression (a) (a))))
