(library (test helper sph lang docl env test)
  (export
    docl-list
    docl-text-reverse
    sxhtml-text-reverse)
  (import
    (sph common))

  (define-syntax-rule (docl-list nesting-depth docl-state a ...) (list a ...))
  (define (docl-text-reverse nesting-depth docl-state a) (string-reverse a))
  (define (sxhtml-text-reverse nesting-depth docl-state . a) (pair (q div) (reverse a))))
