(define-test-module (test module sph io)
  (import
    (sph io))

  (define input-string (open-input-string "line a\nline b\nline c"))

  (test-execute-procedures-lambda
    (port->lines (unquote input-string) ("line a" "line b" "line c"))))
