(import (sph) (sph test) (sph lang docl) (sph lang docl itml))
(define (test-itml->html-sxml inp) (docl-itml-port->html-sxml (open-input-string inp)))

(execute-tests-quasiquote
  (itml->html-sxml
    "\\\\.(+ 1 2)" ((p "\\.(+ 1 2)"))
    "\\.quote\n  (section (\"a\" \"b\")\n  (\"c\"))" ((section ("a" "b") ("c")))
    "\\string-reverse: a b c" ((p "c b a"))
    "keyword:content" ((p "keyword:content"))
    "keyword: content" ((dl (dt "keyword:") (dd "content")))
    "\\.+: 1 (- 2 1) 3" ((p 5))
    "key\\.(+ 3 4)word: con\\.(+ 1 2)tent" ((p "key" " " 7 " " (dl (dt "word:") (dd (section (h3 "con") (p 3) (p "tent"))))))
    "\\.(+ 1 (- 2 1) 3)" ((p 5))
    "\\\\keyword content" ((p "\\keyword content"))
    "\\cons* a b\n  c\n  d\n    e f" ((p "a b" "c" (section (h2 "d") (p "e f"))))
    "\\.+ 1 2\n  (- 3 1)\n" ((p 5))
    "c \\.string-reverse: \"ab\"" ((p "c " "ba"))
))