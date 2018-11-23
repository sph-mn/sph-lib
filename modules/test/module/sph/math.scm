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

  (test-execute-procedures-lambda
    (integer-summands (50 4 2) #t
      ; minimum to large
      (50 4 25) ()
      ; minimum is smallest, int is largest
      (50 50 0) #t
      ; count is smallest
      (50 1 0) #t
      ; int is smallest
      (1 1 0) #t)))
