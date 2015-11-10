(import (sph) (sph test) (sph lang docl) (sph lang docl itml-to-sxml-html))
(define (test-itml->sxml-html inp) (docl-itml-port->sxml-html (open-input-string inp)))

(execute-tests-quasiquote
  (itml->sxml-html
    "\\\\.(+ 1 2)" ("\\.(+ 1 2)")
    "\\.quote\n  (section (\"a\" \"b\")\n  (\"c\"))" ((section ("a" "b") ("c")))
    "\\string-reverse: a b c" ("c b a")
    "keyword:content" ("keyword:content")
    "keyword: content" ("keyword" ": " "content")
    "\\.+: 1 (- 2 1) 3" (5)
    "key\\.(+ 3 4)word: con\\.(+ 1 2)tent" ("key" " " 7 " " "word" ": " "con" 3 "tent")
    "\\.(+ 1 (- 2 1) 3)" (5)
    "\\\\keyword content" ("\\keyword content")
    "\\cons* a b\n  c\n  d\n    e f" ("a b" "c" (section (h2 "d") (div "e f")))
    "\\.+ 1 2\n  (- 3 1)\n" (5)
    "c \\.string-reverse: \"ab\"" ("c " "ba")
))
