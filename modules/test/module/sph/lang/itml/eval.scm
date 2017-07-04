(define-test-module (test module sph lang itml eval)
  (import
    (sph)
    (sph list)
    (rnrs eval)
    (sph lang itml eval)
    (sph lang itml eval shtml))

  (define env
    (apply environment (list-q (sph lang itml eval env shtml) (sph lang itml eval env default))))

  (define-test (itml-shtml-eval arguments)
    (itml-shtml-eval-string (first arguments) (itml-state-create 0 env)))

  (test-execute-procedures-lambda
    (itml-shtml-eval "\\.scm-eval (+ 1 2\n  (- 3 1))\n" ((p 5))
      "\\\\.(+ 1 2)" ((p "\\" ".(+ 1 2)"))
      "keyword:content" ((p "keyword:content"))
      "keyword: content" ((p "keyword" ": " "content"))
      "\\.scm-eval: (+ 1 (- 2 1) 3)" ((p 5))
      "key\\.(scm-eval (+ 3 4))word: con\\.(scm-eval (+ 1 2))tent"
      ((p "key" 7 "word" ": " "con" 3 "tent")) "\\\\keyword content"
      ((p "\\" "keyword content")) "\\scm-list-qq a b\n  c\n  d\n    e f"
      ((p "a b" "c" (section (h2 "d") (div (p "e f")))))
      ;"\\.(docl-eval (+ 1 (- 2 1) 3))" ((p 5))
      ;"c \\.docl-text-reverse: \"ab\"" ((p "c " "ba"))
      ;"\\docl-text-reverse: a b c" ((p "c b a"))
      )))
