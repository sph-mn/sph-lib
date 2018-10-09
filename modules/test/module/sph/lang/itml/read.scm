(define-test-module (test module sph lang itml read)
  (import
    (sph lang itml read)
    (sph lang itml write))

  (define test-env-list-1 (q (a (b ("c" d)))))
  (define test-env-list-2 (q ("a" "e" ("b" ("c d")))))
  (define simple-inline-scm-expression (itml-create-inline-scm-expression test-env-list-1))
  (define simple-line-scm-expression (itml-create-line-scm-expression test-env-list-1))
  (define simple-indent-scm-expression (itml-create-indent-scm-expression test-env-list-1))
  (define simple-inline-text-expression (itml-create-inline-text-expression test-env-list-2))
  (define simple-line-text-expression (itml-create-line-text-expression test-env-list-2))
  (define simple-indent-text-expression (itml-create-indent-text-expression test-env-list-2))
  (define simple-association (itml-create-association "a b" "c d"))
  (define extended-association (itml-create-association "a b" "c " simple-inline-scm-expression))
  (define extended-association-2 (itml-create-association "a" simple-inline-scm-expression))

  (define extended-association-3
    (itml-create-association "a" simple-inline-scm-expression simple-inline-scm-expression))

  (test-execute-procedures-lambda
    (string->itml-parsed "a: #(+ 1 2)" ((association "a" (inline-scm-expression + 1 2)))
      (unquote simple-indent-scm-expression)
      ((unquote (pair (q indent-scm-expression) test-env-list-1))) "#a b\nc\nd"
      ((indent-scm-expression a b) "c" "d") "a\n  #b\n  c"
      (("a" "#b" "c")) "#a b\n  c\n  d"
      ((indent-scm-expression a b c d))
      ; indent-scm
      "#a\n  b\n  \"c\"\n    d" ((indent-scm-expression a b ("c" d)))
      "#a\n  \"c\"\n    d" ((indent-scm-expression a ("c" d)))
      "#a b c\n  \"d\"\n    e" ((indent-scm-expression a b c ("d" e)))
      "#scm (+ 1 2\n  (- 3 1))\n" ((indent-scm-expression scm (+ 1 2 (- 3 1))))
      ; inline-scm
      (unquote simple-inline-scm-expression)
      ((unquote (pair (q inline-scm-expression) test-env-list-1)))
      ; line-scm
      (unquote simple-line-scm-expression) ((unquote (pair (q line-scm-expression) test-env-list-1)))
      "#aa: bb cc" ((line-scm-expression aa bb cc))
      ; inline
      (unquote simple-inline-text-expression)
      ((unquote (pair (q inline-text-expression) test-env-list-2)))
      "##(keyword (c (ont#(+ 1 2)ent)))"
      ((inline-text-expression "keyword" ("c" ("ont" (inline-scm-expression + 1 2) "ent"))))
      "##(+ 3 1)" ((inline-text-expression "+" "3" "1"))
      "##(+ 3 (+ 2 1))" ((inline-text-expression "+" "3" ("+ 2 1")))
      ; line
      (unquote simple-line-text-expression) ((line-text-expression "a" "e (b (c d))"))
      "##a: (b (c #(scm (+ 1 2)))) e"
      ((line-text-expression "a" ("(b (c " (inline-scm-expression scm (+ 1 2)) ")) e")))
      "##aa: bb cc" ((line-text-expression "aa" "bb cc"))
      "\\##a b" ((line "##" "a b"))
      ; indent
      (unquote simple-indent-text-expression) ((indent-text-expression "a" "e" ("b" ("c d"))))
      "##a b" ((indent-text-expression "a" "b"))
      "##keyword content\n  on\n  multiple lines"
      ((indent-text-expression "keyword" "content" "on" "multiple lines"))
      ; association
      (unquote simple-association) ((association "a b" "c d"))
      (unquote extended-association) ((association "a b" "c " (inline-scm-expression a (b ("c" d)))))
      (unquote extended-association-2) ((association "a" (inline-scm-expression a (b ("c" d)))))
      (unquote extended-association-3)
      ( (association "a" (inline-scm-expression a (b ("c" d)))
          (inline-scm-expression a (b ("c" d)))))
      "aa: \\#(+ 1 2)" ((association "aa" "#(+ 1 2)"))
      "a b : c" ("a b : c")
      "a b\\: c" ("a b: c")
      "a b: c" ((association "a b" "c"))
      "a b :c" ("a b :c")
      "keyword:content" ("keyword:content")
      "keyword: content" ((association "keyword" "content"))
      "key#(+ 3 4)word: con#(+ 1 2)tent"
      ( (association ("key" (inline-scm-expression + 3 4) "word") "con"
          (inline-scm-expression + 1 2) "tent")))))
