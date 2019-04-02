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
              points))))
      (path (unquote (spline-path-new* (line (10 10)))) (unquote (spline-path-new* (line (10 10)))))))

  (define path-move
    (spline-path-new* (move (2 10)) (line (10 20) (20 50)) (move (30 30)) (line (40 50))))

  (define path-arc (spline-path-new* (arc (100 100) 10)))
  (define path-constant-1 (spline-path-new* (constant (10 20 30))))
  (define path-constant-2 (spline-path-new* (line (10 10)) (constant)))
  (define path-constant-3 (spline-path-constant 10 20 30))

  (define-test (spline-path-modify)
    (spline-path?
      (spline-path-modify path-all #:reverse
        #t #:randomise
        (random-state-from-platform) #:deep
        #t #:shift 2 #:scale 0.2 #:stretch (* 1 (spline-path-end path-all)))))

  (define-test (spline-path->procedure)
    (let (f (spline-path->procedure path-constant-3))
      (and (list? (f 0)) (every equal? (map tail (list (f 0) (f -1000) (f 1000)))))))

  (define-test (spline-path-combine)
    (let*
      ( (path-a (spline-path-new* (line (10 10)))) (path-b (spline-path-new* (line (10 20))))
        (path-ab (spline-path-combine + path-a path-b)))
      (assert-equal (list 5 15) (spline-path 5 path-ab))))

  (define-test (spline-path-repeat)
    (let*
      ( (path (spline-path-modify (spline-path-new* (line (10 10))) #:repeat #t))
        (path2 (spline-path-modify path #:repeat 3)))
      (assert-and (assert-equal (list 15 5) (spline-path 15 path))
        (assert-equal (list 25 5) (spline-path 25 path2))
        (assert-equal (list 35 0) (spline-path 35 path2)))))

  (test-execute-procedures-lambda (spline-path? (unquote path-all) #t (unquote path-constant-3) #t)
    (spline-path-constant? ((unquote path-constant-1)) #t
      ((unquote path-constant-2)) #f ((unquote path-constant-3)) #t)
    (spline-path-infinite? ((unquote path-constant-1)) #t ((unquote path-constant-2)) #t)
    (spline-path
      ; test move
      (0 (unquote path-move)) (0 0)
      (1 (unquote path-move)) (1 0)
      (2 (unquote path-move)) (2 10)
      (25 (unquote path-move)) (25 0)
      (35 (unquote path-move)) (35 40)
      ; test arc
      (20 (unquote path-arc)) (20 -19.84011233337104)
      ; test constant
      (0 (unquote path-constant-1)) (0 0 0)
      (11 (unquote path-constant-1)) (11 20 30)
      (1000 (unquote path-constant-1)) (1000 20 30)
      (5 (unquote path-constant-2)) (5 5)
      (100 (unquote path-constant-2)) (100 10)
      ; test before/after
      (0 (unquote path-all)) (0 0)
      (1000 (unquote path-all)) (1000 0)
      ; test segment types
      (10 (unquote path-all)) (10 20)
      (11 (unquote path-all)) (11 23)
      (35 (unquote path-all)) (35 105/4)
      (45 (unquote path-all)) (45 2225/108)
      (75 (unquote path-all)) (75 24.954778489117004)
      (105 (unquote path-all)) (105 105/2)
      (115 (unquote path-all)) (115 5)
      (125 (unquote path-all)) (125 5)
      ; test random access
      (45 (unquote path-all)) (45 2225/108)
      (11 (unquote path-all)) (11 23) (45 (unquote path-all)) (45 2225/108))
    (spline-path-modify) (spline-path->procedure) (spline-path-combine) (spline-path-repeat)))
