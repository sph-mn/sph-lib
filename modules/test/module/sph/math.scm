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

  (define path-all
    (spline-path-new* (move (2 10)) (line (10 20) (20 50))
      (bezier (30 10) (40 40) (50 10)) (catmull-rom 0 (60 10) (70 40) (80 10) (90 50))
      (custom-simple (100 0)
        (unquote (l (time start end) (list time (+ (second start) (sin (* time (/ (* 2 pi) 10))))))))
      (custom
        (unquote
          (l (segments config start end)
            (list (list (vector start end (l (t) (list t (+ (second start) (/ t 2)))))) end)))
        (110 0))))

  (define path-move
    (spline-path-new* (move (2 10)) (line (10 20) (20 50)) (move (30 30)) (line (40 50))))

  (define path-constant-1 (spline-path-new* (constant (10 20 30))))
  (define path-constant-2 (spline-path-new* (move (0 10)) (constant)))

  (test-execute-procedures-lambda
    (spline-path
      ; test move
      (0 (unquote path-move)) (0 0)
      (1 (unquote path-move)) (1 0)
      (25 (unquote path-move)) (25 0)
      (35 (unquote path-move)) (35 40)
      ; test constant
      (0 (unquote path-constant-1)) (0 0 0)
      (11 (unquote path-constant-1)) (11 20 30)
      (1000 (unquote path-constant-1)) (1000 20 30)
      (0 (unquote path-constant-2)) (0 10)
      (100 (unquote path-constant-2)) (100 10)
      ; test before/after
      (0 (unquote path-all)) (0 0)
      (1000 (unquote path-all)) (1000 0)
      ; test segment types
      (10 (unquote path-all)) (10 20)
      (11 (unquote path-all)) (11 23)
      (35 (unquote path-all)) (35 105/4)
      (45 (unquote path-all)) (45 2225/108)
      (75 (unquote path-all)) (75.0725039934904 24.954778489117004)
      (105 (unquote path-all)) (105 105/2)
      ; test random access
      (45 (unquote path-all)) (45 2225/108)
      (11 (unquote path-all)) (11 23) (45 (unquote path-all)) (45 2225/108))
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
