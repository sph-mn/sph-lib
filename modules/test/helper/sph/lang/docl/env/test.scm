(library (test helper sph lang docl env test)
  (export
    docl-list
    docl-text-reverse
    sxml-html-text-reverse)
  (import
    (sph common))

  (define-syntax-rule (docl-list nesting-depth docl-state a ...) (list a ...))
  (define (docl-text-reverse nesting-depth docl-state a) (string-reverse a))
  (define (sxml-html-text-reverse nesting-depth docl-state . a) (pair (q div) (reverse a))))
