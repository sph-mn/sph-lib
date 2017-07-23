(define-test-module (test module sph lang indent-syntax)
  (import
    (sph lang indent-syntax)
    (sph tree))

  (test-execute-procedures-lambda
    (prefix-tree->indent-tree ((("a" "b" "c")) 0 "-") "a\n-b\n-c"
      ((("a" ("b" ("c" "d") "e"))) 0 "-") "a\n-b\n--c\n---d\n--e"
      ((("a" ("b" ("c") "d" "e"))) 0 "-") "a\n-b\n---c\n--d\n--e")))
