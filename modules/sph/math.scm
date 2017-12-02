(library (sph math)
  (export
    bezier-curve
    bezier-curve-cubic
    catmull-rom-spline)
  (import
    (sph))

  (define (catmull-rom-spline n p1 p2 p3 p4)
    "number:0..1 vector vector vector vector -> vector
     return a point between p2 and p3 at fractional offset n
     calculated using catmull rom spline interpolation (probably not centripetal).
     no limit on the dimensions of point vectors"
    (vector-map
      (l (d1 d2 d3 d4)
        (* 0.5
          (+ (* 2 d2) (* n (- d3 d1))
            (* n n (- (+ (* 2 d1) (* 4 d3)) (* 5 d2) d4)) (* n n n (- (+ (* 3 d2) d4) (* 3 d3) d1)))))
      p1 p2 p3 p4))

  (define (bezier-curve n . points)
    "number:0..1 (#(number ...) ...) ... -> (#(number ...) ...)
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
        (l (d1 d2 d3 d4) (+ (* r-cube d1) (* l-square d2) (* r-square d3) (* l-cube d4))) p1 p2 p3 p4))))
