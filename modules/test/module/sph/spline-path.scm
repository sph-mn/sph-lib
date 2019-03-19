(define-test-module (test module sph spline-path)
  (import
    (sph spline-path))

  (define path-all
    (spline-path-new* (move (2 10)) (line (10 20) (20 50))
      (bezier (30 10) (40 40) (50 10)) (catmull-rom (60 10) (70 40) (80 10) (90 50))
      (custom-simple (100 0)
        (unquote
          (l (time points)
            (apply (l (start end) (list time (+ (second start) (sin (* time (/ (* 2 pi) 10))))))
              points))))
      (custom (110 0)
        (unquote
          (l (segments next points)
            (apply
              (l (start end)
                (list
                  (vector start end (l (t) (list t (+ (second start) (/ (+ (first start) t) 2)))))))
              points))))))

  (define path-move
    (spline-path-new* (move (2 10)) (line (10 20) (20 50)) (move (30 30)) (line (40 50))))

  (define path-constant-1 (spline-path-new* (constant (10 20 30))))
  (define path-constant-2 (spline-path-new* (move (0 10)) (constant)))
  (define path-arc (spline-path-new* (arc (100 100) 10)))

  (test-execute-procedures-lambda
    (spline-path
      ; test move
      (0 (unquote path-move)) (0 0)
      (1 (unquote path-move)) (1 0)
      (2 (unquote path-move)) (2 10)
      (25 (unquote path-move)) (25 0)
      (35 (unquote path-move)) (35 40)
      ; test constant
      (0 (unquote path-constant-1)) (0 0 0)
      (11 (unquote path-constant-1)) (11 20 30)
      (1000 (unquote path-constant-1)) (1000 20 30)
      (0 (unquote path-constant-2)) (0 10)
      (100 (unquote path-constant-2)) (100 10)
      ; test arc
      (20 (unquote path-arc)) (38.93841289587629 -19.84011233337104)
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
      (11 (unquote path-all)) (11 23) (45 (unquote path-all)) (45 2225/108))))
