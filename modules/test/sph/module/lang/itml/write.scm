(import (sph common) (sph test) (sph lang itml) (sph lang itml write))

#;(define (test-itml-parsed->itml inp exp)
  (let (r (itml-parsed->itml (string->itml-parsed inp) 0))
    (if (string? exp) (equal? r exp) (equal? r inp))))

(define test-env-list-1 (q (a (b ("c" d)))))

(execute-tests-quasiquote
  (itml-create-inline-scm-expr ((unquote test-env-list-1)) "\\.(a (b (\"c\" d)))")
  (itml-create-line-scm-expr ((unquote test-env-list-1)) "\\.a: (b (\"c\" d))")
  (itml-create-indent-scm-expr ((unquote test-env-list-1)) "\\.a b\n  \"c\"\n    d")
  #;(itml-parsed->itml

    ;inline-scm
    ;"\\.(scm (+ 1 2) (+ 3 4))" #t
    ;line-scm
    ;"\\.scm: (+ 1 2) (+ 3 4)" #t
    ;indent-scm
    ;"\\.scm\n  (+ 1 2)\n    (+ 3 4)" #t
    ;association
    ;"aa bb: cc dd" #t

    ))
