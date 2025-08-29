(define-test-module (test module sph math)
  (import (srfi srfi-1) (sph math) (sph list) (sph number))

  (define random-state (random-state-from-platform))

  (define-test (integer-summands inp exp)
    (every identity
      (apply
        (l (int count minimum)
          (map-integers 2000
            (l a
              (let (b (integer-summands int count minimum random-state))
                (if (equal? exp b) exp
                  (assert-and (assert-true "count as expected" (= count (length b)))
                    (assert-true "sum is int" (= int (apply + b)))
                    (assert-true "no value smaller than minimum" (every (l (a) (>= a minimum)) b))))))))
        inp)))

  (define-test (taylor-series-sin inp exp)
    (let (out (taylor-series-sin (first inp) 20)) (if (float-nearly-equal out exp 1.0e-14) exp out)))

  (define-test (bessel inp exp)
    (let (out (apply (l (order x) (bessel order x 30)) inp))
      (if (float-nearly-equal out exp 1.0e-12) exp out)))

  (define list-a (list 1 3 3 6 7 8 9))

  (test-execute-procedures-lambda
    (bessel (0 2.404825557695773) 0
      (0 5.520078110286311) 0
      (0 8.653727912911013) 0
      (1 3.8317059702075125) 0
      (1 7.015586669815619) 0
      (1 10.173468135062722) 0
      (2 5.135622301840683) 0
      (2 8.417244140399864) 0
      (2 11.619841172149059) 0
      (3 6.380161895923983) 0
      (3 9.76102312998167) 0
      (3 13.015200721698434) 0 (0 3.0) -0.26005195490193334 (7 8.0) 0.32058907797982616)
    (factorial (0) 1 (1) 1 (2) 2 (3) 6 (4) 24 (12) 479001600)
    (taylor-series-sin (unquote pi) 0.0
      (unquote (/ pi 2)) 1.0 (unquote (/ pi 6)) 0.5000000000000001)
    (relative-change (2 3) 1/2
      (2 6) 2 (3 2) -1/3 (6 2) -2/3 (0 4) 4 (4 0) -4 (4 4) 0 (1 4) 3 (4 1) -3/4)
    (integer-summands (50 4 2) #t
      ; minimum to large
      (50 4 25) ()
      ; minimum is smallest, int is largest
      (50 50 0) #t
      ; count is smallest
      (50 1 0) #t
      ; int is smallest
      (1 1 0) #t)
    (percent (3 200) 3/2 (200 200) 100 (0 200) 0)
    (absolute-difference (1 3) 2 (3 1) 2 (-1 -3) 2 (-3 -1) 2 (3 -1) 4 (-1 3) 4)
    (list-average ((1 2 3 4)) 5/2) (list-center-of-mass ((unquote list-a)) 149/37)
    (list-median ((unquote list-a)) 6) (list-range ((unquote list-a)) 8)
    (list-mode ((unquote list-a)) 3)))
