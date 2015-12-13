(define-test-module (test module sph lang indent-syntax)
  (import
    (sph lang indent-syntax)
    (sph tree))

  #;(define (test-prefix-tree-conversion)
    (denoted-tree->prefix-tree (read-space-indent-tree->denoted-tree (open-input-string "a\n  b\nc"))))

  (lambda (settings) #t)
  #;(test-execute-procedures-lambda
    (prefix-tree->indent-tree-string
      (("a" "b" "c")) "a\n-b\n-c"
      (("a" ("b" ("c" "d") "e"))) "a\n-b\n--c\n---d\n--e"
      (("a" ("b" ("c") "d" "e"))) "a")))
