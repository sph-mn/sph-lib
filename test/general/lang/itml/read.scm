(import (sph) (sph test) (sph lang itml read) (sph lang itml write))

;nested with each other
;01 01 010 011

(define test-env-list-1 (q (a (b ("c" d)))))
(define test-env-list-2 (q ("a" ("b" ("c d")))))
(define simple-inline-scm-expr (itml-create-inline-scm-expr test-env-list-1))
(define simple-line-scm-expr (itml-create-line-scm-expr test-env-list-1))
(define simple-indent-scm-expr (itml-create-indent-scm-expr test-env-list-1))
(define simple-inline-expr (itml-create-inline-expr test-env-list-2))
(define simple-line-expr (itml-create-inline-expr test-env-list-2))
(define simple-indent-expr (itml-create-inline-expr test-env-list-2))

(execute-tests-quasiquote
  (string->itml-parsed
    ;inline-scm
    (unquote simple-inline-scm-expr) ((unquote (pair (q inline-scm-expr) test-env-list-1)))
    ;line-scm
    (unquote simple-line-scm-expr) ((unquote (pair (q line-scm-expr) test-env-list-1)))
    ;indent-scm
    (unquote simple-indent-scm-expr) ((unquote (pair (q indent-scm-expr) test-env-list-1)))
    ;inline
    (unquote simple-inline-expr) ((unquote (pair (q inline-expr) test-env-list-2)))
    ;line
    ;indent
    ;association
    #;(
    "aa: \\\\.(+ 1 2)" ((association "aa" "\\.(+ 1 2)"))
    "\\.keyword content\non\nmultiple lines\n" (indent-scm-expr "keyword" "content" "on" "multiple lines")
    "\\.keyword content\n  (a b)\n  \\.(+ 1 2) \"c\"" ((indent-scm-expr "keyword" "content" "(a b)" "\\.(+ 1 2) \"c\""))
    "\\keyword: content on line" ((line-expr "keyword" "content on line"))
    "a b : c" ("a b : c")
    "a b\\: c" ("a b: c")
    "a b: c" ((association "a b" "c"))
    "a b :c" ("a b :c")
    "\\keyword content\n  on\n  multiple lines" ((indent-expr "keyword" "content" "on" "multiple lines"))
    "\\(keyword (c (ont\\.(+ 1 2)ent)))" ((inline-expr "keyword" ("c " ("ont" (inline-scm-expr "(+ 1 2)") "ent"))))
    "\\(+ 3 1)" ((inline-expr "+" "3" "1"))
    "\\(+ 3 (+ 2 1))" ((inline-expr "+" "3" ("+ 2 1")))
    "\\.keyword: content on line" ((line-scm-expr "keyword" "content on line"))
    "\\\\keyword content" ((line "\\keyword content"))
    "keyword:content" ("keyword:content")
    "keyword: content" ((association "keyword" "content"))
    "key\\.(+ 3 4)word: con\\.(+ 1 2)tent" ((line "key" (inline-scm-expr "(+ 3 4)") (association "word" "con" (inline-scm-expr "(+ 1 2)") "tent")))
    )

    ))
