(define-test-module (test module sph lang docl)
  (import (rnrs eval) (sph lang docl) (sph lang docl itml-to-sxml-html))

  (define docl-itml-env-test
    (apply environment (q (test helper sph lang docl env test))
      (q (sph lang docl itml-to-sxml-html)) docl-itml-env-sxml-html-module-names))

  (define-test (itml->sxml-html arguments)
    (docl-itml-string->sxml-html (first arguments) 0 docl-state-empty docl-itml-env-test))

  (test-execute-procedures-lambda
    (itml->sxml-html
      "\\\\.(+ 1 2)" ((p "\\" ".(+ 1 2)"))
      "\\docl-text-reverse: a b c" ((p "c b a"))
      "keyword:content" ((p "keyword:content"))
      "keyword: content" ((p "keyword" ": " "content"))
      "\\.scm: (+ 1 (- 2 1) 3)" ((p 5))
      "key\\.(scm (+ 3 4))word: con\\.(scm (+ 1 2))tent"
      ((p "key" 7  "word" ": " "con" 3 "tent"))
      "\\.(scm (+ 1 (- 2 1) 3))" ((p 5))
      "\\\\keyword content" ((p "\\keyword content"))
      "\\docl-list a b\n  c\n  d\n    e f" ((p "a b" "c" (section (h2 "d") (div (p "e f")))))
      "\\.scm (+ 1 2\n  (- 3 1))\n" ((p 5))
      "c \\.docl-text-reverse: \"ab\"" ((p "c " "ba")))))
