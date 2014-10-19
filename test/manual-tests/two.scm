(import
  (sph)
  (sph two)
  (sph test)
  (sph tree))

(define (test-prefix-tree->indent-tree-string inp)
  (prefix-tree->indent-tree-string inp 0 "-"))

(debug-log (denoted-tree->prefix-tree (read-space-indent-tree-string->denoted-tree (open-input-string "a\n   b\nc"))))

#;(execute-tests-quasiquote
  (prefix-tree->indent-tree-string
    (("a" "b" "c")) "a\n-b\n-c"
    (("a" ("b" ("c" "d") "e"))) "a\n-b\n--c\n---d\n--e"
    (("a" ("b" ("c") "d" "e"))) "a"

    ))
