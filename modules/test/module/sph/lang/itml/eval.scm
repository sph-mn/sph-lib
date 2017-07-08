(define-test-module (test module sph lang itml eval)
  (import
    (sph)
    (sph list)
    (rnrs eval)
    (sph lang itml eval)
    (sph lang itml eval shtml)
    (sph lang itml eval plaintext))

  (define env-shtml
    (apply environment (list-q (sph lang itml eval env shtml) (sph lang itml eval env default))))

  (define env-plaintext
    (apply environment (list-q (sph lang itml eval env plaintext) (sph lang itml eval env default))))

  (define-test (itml-shtml-eval arguments)
    (itml-shtml-eval-string (first arguments) (itml-state-create 0 env-shtml)))

  (define-test (itml-plaintext-eval arguments)
    (itml-plaintext-eval-string (first arguments) (itml-state-create 0 env-plaintext)))

  (test-execute-procedures-lambda
    (itml-plaintext-eval "\\.scm-eval (+ 1 2\n  (- 3 1))\n" "5"
      "\\\\.(+ 1 2)" "\\.(+ 1 2)"
      "keyword:content" "keyword:content"
      "keyword: content" "keyword: content"
      "\\.scm-eval: (+ 1 (- 2 1) 3)" "5"
      "key\\.(scm-eval (+ 3 4))word: con\\.(scm-eval (+ 1 2))tent" "key 7 word: con 3 tent"
      "\\\\keyword content" "\\keyword content"
      "\\.(scm-eval (+ 1 (- 2 1) 3))" "5" "\\scm-list-qq a b\n  c\n  d\n    e f" "(a b c (d e f))")
    (itml-shtml-eval "\\.scm-eval (+ 1 2\n  (- 3 1))\n" ((p 5))
      "\\\\.(+ 1 2)" ((p "\\" ".(+ 1 2)"))
      "keyword:content" ((p "keyword:content"))
      "keyword: content" ((p "keyword" ": " "content"))
      "\\.scm-eval: (+ 1 (- 2 1) 3)" ((p 5))
      "key\\.(scm-eval (+ 3 4))word: con\\.(scm-eval (+ 1 2))tent"
      ((p "key" 7 "word" ": " "con" 3 "tent")) "\\\\keyword content"
      ((p "\\" "keyword content")) "\\scm-list-qq a b\n  c\n  d\n    e f"
      ((p "a b" "c" (section (h2 "d") (div (p "e f"))))) "\\.(scm-eval (+ 1 (- 2 1) 3))" ((p 5)))))
