(import (sph) (sph string) (sph filesystem) (sph test performance))
(define (create-random-slashes) (if (odd? (+ 1 (random 5))) (string-multiply "/" (random 6)) ""))

(define path-parts
  (map (l (ele) (string-append (create-random-slashes) ele (create-random-slashes)))
    (make-list 22 "abcdefghijklmn")))

(define (test-path-append n) (apply path-append path-parts))
(define (test-path-append* n) (apply path-append* path-parts))
(evaluate-performance 6 test-path-append test-path-append*)