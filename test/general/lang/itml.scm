(import (sph common) (sph test) (sph lang itml) (sph lang parser itml))

(define (test-itml-parsed->itml inp exp)
  (let (r (itml-parsed->itml (string->itml-parsed inp) 0))
    (if (string? exp) (equal? r exp) (equal? r inp))))

(execute-tests-quasiquote
  (itml-parsed->itml
    ;inline-scm
    "\\.(scm (+ 1 2) (+ 3 4))" #t
    ;line-scm
    "\\.scm: (+ 1 2) (+ 3 4)" #t
    ;indent-scm
    "\\.scm\n  (+ 1 2)\n    (+ 3 4)" #t
    ;association
    "aa bb: cc dd" #t

    ))
