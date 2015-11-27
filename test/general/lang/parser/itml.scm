(import (sph) (sph test) (sph lang parser itml))

;nested with each other
;01 01 010 011

(execute-tests-quasiquote
  (string->itml-parsed
    ;inline-scm
    "\\.(a (b (\"c\" d)))" ((inline-scm-expr a (b ("c" d))))
    ;line-scm
    "\\.a: \"b\" (c)" ((line-scm-expr a "b" (c)))
    ;indent-scm
    "\\.a\n  \"b\"\n    c" ((indent-scm-expr a ("b" c)))
    ;inline
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
