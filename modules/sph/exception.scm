(library (sph exception)
  (export
    exception-always
    exception-intercept
    exception-intercept-if)
  (import
    (rnrs exceptions)
    (sph)))

;dealing with rnrs exceptions

(define-syntax-rule (exception-intercept on-exception expression)
  ;"evaluate \"on-exception\" only when an exception occurs in expression.
  ;the result is the result of \"expression\" if no exception occurred"
  (with-exception-handler (l (a) on-exception (raise a)) (thunk expression)))

(define-syntax-rule (exception-intercept-if expression exception no-exception)
  ;"evaluate \"exception\" when an exception occurs in expression. evaluate \"no-exception\" if no exception occurred.
  ;the result is the result of \"expression\" if no exception occurred"
  (let (r (exception-intercept exception expression)) no-exception r))

(define-syntax-rule (exception-always always expression)
  ;"evaluate \"always\" after expression even when an an exception occurs.
  ;the result is the result of \"expression\" if no exception occurred"
  (let (r (exception-intercept always expression)) always r))
