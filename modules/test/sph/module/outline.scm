(import (sph test-old) (sph) (sph lang parser outline))
(define (test-read-outline inp exp) (read-outline (open-input-string inp) #\=))

(execute-tests-quasiquote
  (read-outline
    "= 1\na\nb\n== 11\nc\n== 12\n=== 121\nd\ne\n= 2\nf\ng"
    (("1" "a" "b" ("11" "c") ("12" ("121" "d" "e"))) ("2" "f" "g"))
    "= 1\n=== 2\n= 3"
    (("1" ("2")) ("3"))
    "= 1\n= 2\n="
    (("1") ("2" "="))
    "= a\nb\n==c\nd\ne\n===f\ng\nh"
    (("a" "b" ("c" "d" "e" ("f" "g" "h"))))
    "a\n= b\n= c\n= d\ne"
    ("a" ("b") ("c") ("d" "e"))
    "a\nb\nc"
    ("a" "b" "c")))