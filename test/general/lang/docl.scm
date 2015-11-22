(import (rnrs eval) (sph) (sph test) (sph lang docl) (sph lang docl itml-to-sxml-html))

(define docl-itml-env-test
  (apply environment (q (test sph lang docl env test))
    (q (sph lang docl itml-to-sxml-html)) docl-itml-env-sxml-html-module-names))

(define (test-itml->sxml-html inp)
  (docl-itml-string->sxml-html inp 0 docl-state-empty docl-itml-env-test))

(execute-tests-quasiquote
  (itml->sxml-html "\\\\.(+ 1 2)" ((p "\\.(+ 1 2)"))
    "\\docl-text-reverse: a b c" ((p "c b a"))
    "keyword:content" ((p "keyword:content"))
    "keyword: content" ((p "keyword" ": " "content"))
    "\\.sxml: (+ 1 (- 2 1) 3)" ((p 5))
    "key\\.(sxml (+ 3 4))word: con\\.(sxml (+ 1 2))tent"
    ((p "key" " " 7 " " "word" ": " "con" 3 "tent")) "\\.(sxml (+ 1 (- 2 1) 3))"
    ((p 5)) "\\\\keyword content"
    ((p "\\keyword content")) "\\docl-list a b\n  c\n  d\n    e f"
    ((p "a b" "c" (section (h2 "d") (div (p "e f"))))) "\\.sxml (+ 1 2\n  (- 3 1))\n"
    ((p 5)) "c \\.docl-text-reverse: \"ab\"" ((p "c " "ba"))))
