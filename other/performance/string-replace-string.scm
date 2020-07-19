(use-modules (srfi srfi-1)
  (ice-9 regex) (sph) (sph string) (sph other) (sph test performance))

(define regular-string (string-multiply "12" 300))

(define varied-string
  (list->string (map! integer->char (map! (l (ele) (+ 10 (random ele))) (make-list 300 200)))))

(define (test-regexp-regular n) (regexp-replace regular-string "12" "121212"))
(define (test-regexp-varied n) (regexp-replace varied-string "12" "121212"))
(define (test-replace-regular n) (string-replace-string regular-string "12" "121212"))
(define (test-replace-varied n) (string-replace-string varied-string "12" "121212"))

(evaluate-performance 2 test-regexp-regular
  test-regexp-varied test-replace-regular test-replace-varied)
