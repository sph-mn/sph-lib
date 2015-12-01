(import (rnrs bytevectors) (srfi srfi-1)
  (ice-9 regex) (sph) (sph string) (sph one) (sph test performance))

(define regular-string (string-multiply "12" 300))

(define varied-string
  (list->string (map! integer->char (map! (l (ele) (+ 10 (random ele))) (make-list 300 200)))))

(define (test-match-regular n) (regexp-replace regular-string "12" "121212"))
(define (test-match-varied n) (regexp-replace varied-string "12" "121212"))
(define (test-replace-regular n) (string-replace-string regular-string "12" "121212"))
(define (test-replace-varied n) (string-replace-string varied-string "12" "121212"))

(evaluate-performance 2 test-match-regular
  test-match-varied test-replace-regular test-replace-varied)