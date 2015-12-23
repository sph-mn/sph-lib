(define-test-module (test module sph lang itml read)
  (import
    (sph lang itml read)
    (sph lang itml write))

  (define test-env-list-1 (q (a (b ("c" d)))))
  (define test-env-list-2 (q ("a" "e" ("b" ("c d")))))
  (define simple-inline-scm-expr (itml-create-inline-scm-expr test-env-list-1))
  (define simple-line-scm-expr (itml-create-line-scm-expr test-env-list-1))
  (define simple-indent-scm-expr (itml-create-indent-scm-expr test-env-list-1))
  (define simple-inline-expr (itml-create-inline-expr test-env-list-2))
  (define simple-line-expr (itml-create-line-expr test-env-list-2))
  (define simple-indent-expr (itml-create-indent-expr test-env-list-2))
  (define simple-association (itml-create-association "a b" "c d"))
  (define extended-association (itml-create-association "a b" "c " simple-inline-scm-expr))
  (define extended-association-2 (itml-create-association "a" simple-inline-scm-expr))

  (define extended-association-3
    (itml-create-association "a" simple-inline-scm-expr simple-inline-scm-expr))

  (test-execute-procedures-lambda
    (string->itml-parsed
      "a: \\.(+ 1 2)" ((association "a" (inline-scm-expr + 1 2)))
      (unquote simple-indent-scm-expr) ((unquote (pair (q indent-scm-expr) test-env-list-1)))
      "\\.a b\nc\nd" ((indent-scm-expr a b) "c" "d")
      "a\n  \\.b\n  c" (("a" "\\.b" "c"))
      "\\.a b\n  c\n  d" ((indent-scm-expr a b c d))
      ;indent-scm
      "\\.a\n  b\n  \"c\"\n    d" ((indent-scm-expr a b ("c" d)))
      "\\.a\n  \"c\"\n    d" ((indent-scm-expr a ("c" d)))
      "\\.a b c\n  \"d\"\n    e" ((indent-scm-expr a b c ("d" e)))
      "\\.scm (+ 1 2\n  (- 3 1))\n" ((indent-scm-expr scm (+ 1 2 (- 3 1))))
      ;inline-scm
      (unquote simple-inline-scm-expr) ((unquote (pair (q inline-scm-expr) test-env-list-1)))
      ;line-scm
      (unquote simple-line-scm-expr) ((unquote (pair (q line-scm-expr) test-env-list-1)))
      "\\.aa: bb cc" ((line-scm-expr aa bb cc))
      ;inline
      (unquote simple-inline-expr) ((unquote (pair (q inline-expr) test-env-list-2)))
      "\\(keyword (c (ont\\.(+ 1 2)ent)))" ((inline-expr "keyword" ("c" ("ont" (inline-scm-expr + 1 2) "ent"))))
      "\\(+ 3 1)" ((inline-expr "+" "3" "1"))
      "\\(+ 3 (+ 2 1))" ((inline-expr "+" "3" ("+ 2 1")))
      ;line
      (unquote simple-line-expr) ((line-expr "a" "e (b (c d))"))
      "\\a: (b (c \\.(scm (+ 1 2)))) e" ((line-expr "a" ("(b (c " (inline-scm-expr scm (+ 1 2)) ")) e")))
      "\\aa: bb cc" ((line-expr "aa" "bb cc"))
      "\\\\a b" ((line "\\" "a b"))
      ;indent
      (unquote simple-indent-expr) ((indent-expr "a" "e" ("b" ("c d"))))
      "\\a b" ((indent-expr "a" "b"))
      "\\keyword content\n  on\n  multiple lines" ((indent-expr "keyword" "content" "on" "multiple lines"))
      ;association
      (unquote simple-association) ((association "a b" "c d"))
      (unquote extended-association) ((association "a b" "c " (inline-scm-expr a (b ("c" d)))))
      (unquote extended-association-2) ((association "a" (inline-scm-expr a  (b ("c" d)))))
      (unquote extended-association-3)
      ((association "a" (inline-scm-expr a (b ("c" d))) (inline-scm-expr a (b ("c" d)))))
      "aa: \\\\.(+ 1 2)" ((association "aa" "\\.(+ 1 2)"))
      "a b : c" ("a b : c")
      "a b\\: c" ("a b: c")
      "a b: c" ((association "a b" "c"))
      "a b :c" ("a b :c")
      "keyword:content" ("keyword:content")
      "keyword: content" ((association "keyword" "content"))
      "key\\.(+ 3 4)word: con\\.(+ 1 2)tent" ((association ("key" (inline-scm-expr + 3 4) "word") "con" (inline-scm-expr + 1 2) "tent"))
      )))
