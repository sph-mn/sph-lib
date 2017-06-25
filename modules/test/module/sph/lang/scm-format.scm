(define-test-module (test module sph lang scm-format)
  (import
    (sph base)
    (sph lang scm-format)
    (sph lang scm-format format)
    (sph lang scm-format transform))

  (define (format-docstring* a indent)
    (format-docstring a (ht-ref scm-format-default-config (q format)) indent))

  (define (s n) (string-multiply " " n))

  (define (example-string a b c)
    "integer integer integer -> string
     creates a string with lines indented by depths a, b and c respectively"
    (string-append "a\n" (s a) "b\n" (s b) "c\n" (s c) "d"))

  (define (example-result . a)
    "like example-string but creates a format-docstring result style string"
    (string-append "\"" (apply example-string a) "\""))

  (define format-docstring-io
    ; (input indent output)
    ; input is passed to example-string, output is passed to example-result
    (list-q ((2 4 2) 0 (1 3 1)) ((2 4 2) 1 (3 5 3))
      ((2 4 2) 2 (5 7 5)) ((2 4 0) 1 (3 5 3))
      ((0 0 0) 0 (1 1 1)) ((0 0 0) 1 (3 3 3)) ((0 0 0) 2 (5 5 5))))

  (define-test (format-docstring)
    "on failure it returns the index format-docstring-io index where it failed"
    (every-or-index
      (l (a)
        (list-bind a (input indent expected)
          (equal? (format-docstring* (apply example-string input) indent)
            (apply example-result expected))))
      format-docstring-io))

  (test-execute-procedures-lambda (format-docstring)))
