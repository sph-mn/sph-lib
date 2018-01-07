(library (sph math)
  (export
    angle-between
    bezier-curve
    bezier-curve-cubic
    catmull-rom-spline
    circle
    ellipse
    elliptical-arc
    golden-ratio
    hermite-interpolation
    line-path
    linear-interpolation
    pi
    point-distance)
  (import
    (sph)
    (sph vector)
    (only (sph alist) alist-q)
    (only (sph list) consecutive))

  (define golden-ratio (/ (+ 1 (sqrt 5)) 2))
  (define pi (* 4 (atan 1)))

  (define (bezier-curve n . points)
    "number:0..1 #(number ...) ... -> #(number ...)
     get a point for a bezier curve at fractional offset n.
     no limit on the number of control points.
     no limit on the dimension of point vectors.
     at least one point must be given.
     uses the \"de casteljau\" algorithm"
    (if (null? (tail points)) (first points)
      (let ((left n) (right (- 1 n)))
        ; length of points is reduced by one for each recursive call
        (apply bezier-curve n
          ; use of pair-fold to check for a next element
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
    "return a point on a circle with given radius at fractional offset n (on the circumference)"
    (let (n (* n 2 pi)) (vector (* radius (cos n)) (* radius (sin n)))))

  (define (ellipse n radius-x radius-y rotation)
    "number:0..1 number number number:0..2pi
     return a point on an ellipse at fractional offset n (on the circumference)"
    ; make n be a fraction of one period
    (let (n (* n 2 pi))
      (vector
        ; multiplied by rotation matrix (((cos angle) (- (sin angle))) ((sin angle) (cos angle)))
        (- (* radius-x (cos n) (cos rotation)) (* radius-y (sin n) (sin rotation)))
        (+ (* radius-x (cos n) (sin rotation)) (* radius-y (sin n) (cos rotation))))))

  (define (point-distance p1 p2)
    (sqrt
      (+ (expt (- (vector-first p2) (vector-first p1)) 2)
        (expt (- (vector-second p2) (vector-second p1)) 2))))

  (define (angle-between p1 p2)
    (let*
      ( (p (+ (* (vector-first p1) (vector-first p2)) (* (vector-second p1) (vector-second p2))))
        (n
          (sqrt
            (* (+ (expt (vector-first p1) 2) (expt (vector-second p1) 2))
              (+ (expt (vector-first p2) 2) (expt (vector-second p2) 2)))))
        (sign
          (if
            (<
              (- (* (vector-first p1) (vector-second p2)) (* (vector-second p1) (vector-first p2))) 0)
            -1 1)))
      (* sign (acos (/ p n)))))

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
            (linear-interpolation sub-n p1 p2))))))

  (define* (elliptical-arc n p1 p2 rx ry #:optional (rotation 0) large-arc sweep)
    "number:0..1 vector vector number number number:radians boolean boolean -> (vector . extra-calculated-values)
     extra-calculated-values: ((symbol:start-angle/end-angle/angle/center/radius-x/radius-y number) ...)
     return a point on an elliptical arc at fractional offset n.
     modeled after the svg path arc command.
     code translated from https://github.com/MadLittleMods/svg-curve-lib"
    ; there seems to be a bug in this version with the sweep-angle
    (cond ((equal? p1 p2) p1) ((or (= 0 rx) (= 0 ry)) (linear-interpolation n p1 p2))
      (else
        (let*
          ( (rx (abs rx)) (ry (abs ry))
            ; following "conversion from endpoint to center parameterization" at
            ; http://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
            ; step 1: compute transformed point
            (dx (/ (- (vector-first p1) (vector-first p2)) 2))
            (dy (/ (- (vector-second p1) (vector-second p2)) 2))
            (transformed-point-x (+ (* dx (cos rotation)) (* dy (sin rotation))))
            (transformed-point-y (- (* dy (cos rotation)) (* dx (sin rotation))))
            ; ensure radii are large enough
            (radii-check
              (+ (/ (expt transformed-point-x 2) (expt rx 2))
                (/ (expt transformed-point-y 2) (expt ry 2))))
            (rx (if (> radii-check 1) (* rx (sqrt radii-check)) rx))
            (ry (if (> radii-check 1) (* ry (sqrt radii-check)) ry))
            ; step 2: compute transformed center
            (c-square-numerator
              (- (* (expt rx 2) (expt ry 2)) (* (expt rx 2) (expt transformed-point-y 2))
                (* (expt ry 2) (expt transformed-point-x 2))))
            (c-square-root-denom
              (+ (* (expt rx 2) (expt transformed-point-y 2))
                (* (expt ry 2) (expt transformed-point-x 2))))
            (c-radicand (/ c-square-numerator c-square-root-denom))
            (c-radicand (if (< c-radicand 0) 0 c-radicand))
            (c-coef (* (sqrt c-radicand) (if (equal? large-arc sweep) -1 1)))
            (transformed-center-x (/ (* c-coef rx transformed-point-y) ry))
            (transformed-center-y (/ (* c-coef (- (* ry transformed-point-x))) rx))
            ; step 3: compute center
            (center
              (vector
                (+
                  (- (* (cos rotation) transformed-center-x)
                    (* (sin rotation) transformed-center-y))
                  (/ (+ (vector-first p1) (vector-first p2)) 2))
                (+ (* (sin rotation) transformed-center-x) (* (cos rotation) transformed-center-y)
                  (/ (+ (vector-second p1) (vector-second p2)) 2))))
            ; step 4: compute start/sweep angles
            ; start angle of the elliptical arc prior to the stretch and rotate operations.
            ; difference between the start and end angles
            (start-vector
              (vector (/ (- transformed-point-x transformed-center-x) rx)
                (/ (- transformed-point-y transformed-center-y) ry)))
            (start-angle (angle-between (vector 1 0) start-vector))
            (end-vector
              (vector (/ (- (- transformed-point-x) transformed-center-x) rx)
                (/ (- (- transformed-point-y) transformed-center-y) ry)))
            (sweep-angle (angle-between start-vector end-vector))
            (sweep-angle
              (if (and (not sweep) (> sweep-angle 0)) (- sweep-angle (* 2 pi))
                (if (and sweep (< sweep-angle 0)) (+ sweep-angle (* 2 pi)) sweep-angle)))
            (sweep-angle (mod sweep-angle (* 2 pi)))
            ; from http://www.w3.org/TR/SVG/implnote.html#ArcParameterizationAlternatives
            (angle (+ start-angle (* n sweep-angle))) (ellipse-component-x (* rx (cos angle)))
            (ellipse-component-y (* ry (sin angle)))
            (point
              (vector
                (+
                  (- (* ellipse-component-x (cos rotation)) (* ellipse-component-y (sin rotation)))
                  (vector-first center))
                (+ (* ellipse-component-x (sin rotation)) (* ellipse-component-y (cos rotation))
                  (vector-second center)))))
          (pair point
            ; include some extra information in the result which might be useful
            (alist-q start-angle start-angle
              end-angle (+ start-angle sweep-angle) angle angle center center radius-x rx radius-y ry)))))))
