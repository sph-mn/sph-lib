(define-test-module (test module sph math)
  (import
    (sph math)
    (sph list))

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

  (define list-a (list 1 3 3 6 7 8 9))

  (test-execute-procedures-lambda
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
