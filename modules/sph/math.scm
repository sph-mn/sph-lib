(library (sph math)
  (export
    bezier-curve
    bezier-curve-cubic
    catmull-rom-spline
    circle
    golden-ratio
    hermite-interpolation
    line-path
    linear-interpolation
    pi)
  (import
    (sph))

  (define golden-ratio (/ (+ 1 (sqrt 5)) 2))
  (define pi (* 4 (atan 1)))

  (define (bezier-curve n . points)
    "number:0..1 (#(number ...) ...) ... -> #(number ...)
     get a point for a bezier curve at fractional offset n.
     no limit on the number of control points.
     no limit on the dimension of point vectors.
     at least one point must be given.
     uses the \"de casteljau\" algorithm"
    (if (null? (tail points)) (first points)
      (let ((left n) (right (- 1 n)))
        ; length of points is reduced by one for each recursive call
        (apply bezier-curve n
          (pair-fold-right
            (l (a result)
              ; ignore last point
              (if (null? (tail a)) result
                (let ((p1 (first a)) (p2 (second a)))
                  (pair
                    (vector-map
                      ; map all point dimensions
                      (l (p1-d p2-d) (+ (* right p1-d) (* left p2-d))) p1 p2)
                    result))))
            null points)))))

  (define (bezier-curve-cubic n p1 p2 p3 p4)
    "number vector ... -> vector
     return coordinates for one point of a cubic 4-point bezier curve at fractional offset n.
     like bezier-curve but optimised for cubic bezier curves.
     the intermediate points between p1 and p4 are the control points.
     there is no limit on the dimensions of point vectors"
    (let*
      ( (left n) (right (- 1 n)) (l-square (* left left))
        (r-square (* right right)) (l-cube (* left l-square))
        (r-cube (* right r-square)) (l-square (* 3 right l-square)) (r-square (* 3 left r-square)))
      (apply vector-map
        ; map all point dimensions
        (l (d1 d2 d3 d4) (+ (* r-cube d1) (* l-square d2) (* r-square d3) (* l-cube d4))) p1 p2 p3 p4)))

  (define (catmull-rom-spline n p1 p2 p3 p4)
    "number:0..1 vector vector vector vector -> vector
     return a point between p2 and p3 at fractional offset n
     calculated using catmull rom spline interpolation (probably not centripetal).
     no limit on the dimensions of point vectors.
     catmull rom splines have c1 continuity, local control, and interpolation,
     but do not lie within the convex hull of their control points (p1 and p4)"
    (vector-map
      (l (d1 d2 d3 d4)
        (* 0.5
          (+ (* 2 d2) (* n (- d3 d1))
            (* n n (- (+ (* 2 d1) (* 4 d3)) (* 5 d2) d4)) (* n n n (- (+ (* 3 d2) d4) (* 3 d3) d1)))))
      p1 p2 p3 p4))

  (define (hermite-interpolation n tension bias p1 p2 p3 p4)
    "number:0..1 number-1..1 symbol:-1..1 vector vector vector vector -> vector
     tension: -1 low, 0 normal, 1 high
     bias: negative: towards p1, zero: even, positive: towards p4"
    (vector-map
      (l (d1 d2 d3 d4)
        (let*
          ( (n-square (* n n)) (n-cube (* n n-square))
            (diff1
              (+ (/ (* (- d2 d1) (+ 1 bias) (- 1 tension)) 2)
                (/ (* (- d3 d2) (- 1 bias) (- 1 tension)) 2)))
            (diff2
              (+ (/ (* (- d3 d2) (+ 1 bias) (- 1 tension)) 2)
                (/ (* (- d4 d3) (- 1 bias) (- 1 tension)) 2)))
            (a1 (+ 1 (- (* 2 n-cube) (* 3 n-square)))) (a2 (+ n (- n-cube (* 2 n-square))))
            (a3 (- n-cube n-square)) (a4 (- (* 3 n-square) (* 2 n-cube))))
          (+ (* a1 d2) (* a2 diff1) (* a3 diff2) (* a4 d3))))
      p1 p2 p3 p4))

  (define (linear-interpolation n p1 p2)
    "number:0..1 vector vector -> point
     return a point on a straight line between p1 and p2 at fractional offset n"
    (vector-map (l (d1 d2) (+ (* d2 n) (* d1 (- 1 n)))) p1 p2))

  (define (circle n radius)
    "return a point on a circle with given radius at fractional offset (on the circumference) n"
    (let (n (* n (* 2 sp-pi))) (vector (* radius (cos n)) (* radius (sin n)))))

  (define (line-path n . points)
    "number:0..1 vector ... -> vector
     return a point at fractional offset n on a path constructed from given points connected by straight lines.
     costly but stateless algorithm"
    ; find two points for the current line then find the current point on the line
    (let*
      ( (x-values (map vector-first points)) (x-max (apply max x-values))
        (x-min (apply min x-values)) (x (* n (- x-max x-min)))
        (sub (consecutive (l (a) (<= (vector-first a) x)) points pair)))
      (let (p1 (last (first sub)))
        (if (null? (tail sub)) p1
          (let*
            ( (p2 (first (tail sub)))
              (sub-n (/ (- x (vector-first p1)) (- (vector-first p2) (vector-first p1)))))
            (linear-interpolation sub-n p1 p2)))))))
