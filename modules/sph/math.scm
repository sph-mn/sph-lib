(library (sph math)
  (export
    absolute-difference
    absolute-threshold
    angle-between
    arithmetic-mean
    bezier-curve
    bezier-curve-cubic
    catmull-rom-interpolate-f
    catmull-rom-path
    circle
    complex-from-magnitude-and-imaginary
    cusum
    ellipse
    elliptical-arc
    golden-ratio
    hermite-interpolate
    integer-summands
    line-path
    linearly-interpolate
    list-average
    list-center-of-mass
    list-median
    list-mode
    list-range
    log2
    percent
    pi
    relative-change
    scale-to-mean
    spline-path
    spline-path-new
    spline-path-new*
    vector-linearly-interpolate)
  (import
    (rnrs exceptions)
    (sph)
    (sph number)
    (sph vector)
    (only (guile)
      make-list
      *random-state*
      random)
    (only (rnrs sorting) list-sort)
    (only (sph alist) alist-q)
    (only (sph list)
      consecutive
      count-value
      map-integers
      map-segments
      map-with-index
      list-sort-with-accessor)
    (only (sph list) pair-fold-multiple))

  (define sph-math-description "splines, statistics and more")
  (define golden-ratio (/ (+ 1 (sqrt 5)) 2))
  (define pi (* 4 (atan 1)))
  (define (log2 b) "calculate the base two logarithm for b" (/ (log b) (log 2)))

  (define (absolute-threshold b limit) "return zero if the absolute value of b is below limit"
    (if (< (abs b) limit) 0 b))

  (define (scale-to-mean mean b)
    "number (number ...) -> (number ...)
     scale the numbers in b to the given mean while keeping ratios between values the same"
    (if (zero? mean) (make-list (length b) 0)
      (let (ratio (/ (arithmetic-mean b) mean)) (if (zero? ratio) b (map (l (b) (/ b ratio)) b)))))

  (define (arithmetic-mean a)
    "(number ...) -> number
     calculate the arithmetic mean of the given numbers"
    (/ (apply + a) (length a)))

  (define list-average arithmetic-mean)

  (define (absolute-difference n-1 n-2)
    "number number -> number
     give the non-negative difference of two numbers"
    (abs (- n-1 n-2)))

  (define (cusum a . b)
    "calculate cumulative sums from the given numbers.
     (a b c ...) -> (a (+ a b) (+ a b c) ...)"
    (pair a (if (null? b) null (apply cusum (+ a (first b)) (tail b)))))

  (define (list-center-of-mass a)
    "(number ...) -> number
     the distribution of mass is balanced around the center of mass and the average
     of the weighted position coordinates of the distributed mass defines its coordinates.
     c = sum(n * x(n)) / sum(x(n))"
    (/ (apply + (map-with-index (l (index a) (* index a)) a)) (apply + a)))

  (define (list-median a)
    "(number ...) -> number
     return the median value of list. the median is the value separating the
     higher half from the lower half in a sorted list of samples.
     it may be thought of as the \"middle\" value"
    (let ((sorted (list-sort < a)) (size (length a)))
      (if (odd? size) (list-ref sorted (/ (- size 1) 2))
        (let ((index-a (- (/ size 2) 1)) (index-b (/ size 2)))
          (/ (+ (list-ref sorted index-a) (list-ref sorted index-b)) 2)))))

  (define (list-mode a)
    "(number ...) -> number
     return the most common value in list or zero if none repeats"
    (let
      (most-common
        (tail
          (first
            (list-sort-with-accessor > first
              (map (l (b) (pair (count-value b a) b)) (delete-duplicates a))))))
      (if (= 1 most-common) 0 most-common)))

  (define (list-range a)
    "(number ...) -> number
     return the difference of the largest and the smallest value in list"
    (- (apply max a) (apply min a)))

  (define (percent value base) "how many percent is value from base"
    (if (zero? base) 0 (/ (* value 100) base)))

  (define (relative-change a b)
    "number number -> number
     give the relative change between two numbers.
     result times 100 gives the percentage change.
     if a or b is zero then 1 is used in place.
     example: 4 to 1 -> -3/4"
    (/ (- b a) (if (or (zero? a) (zero? b)) 1 a)))

  (define (complex-from-magnitude-and-imaginary m i)
    "create a complex number from a magnitude and the imaginary part of the number"
    ; sqrt gives a complex number if the input value is negative
    (let* ((a (- (* m m) (* i i))) (b (sqrt (abs a))) (c (if (< a 0) (- b) b)))
      (make-rectangular c i)))

  (define* (integer-summands int count minimum #:optional (random-state *random-state*))
    "split an integer int into count numbers equal or greater than minimum whose sum is int.
     # algorithm:
     * get count random numbers of the range minimum to int
     * scale numbers by int divided by sum-of-numbers so to preserve relative differences
     * add either 1 or -1 to every number at random indices until the sum is int"
    ; a new parameter for a custom random function should be added to control probabilities
    (if (> (* count minimum) int) null
      (let*
        ( (numbers (map-integers count (l (a) (+ minimum (random (- int minimum) random-state)))))
          (numbers-sum (apply + numbers)) (scale (if (= 0 numbers-sum) 1 (/ int numbers-sum)))
          (numbers (map (l (a) (round (max minimum (* scale a)))) numbers))
          (deviation (- int (apply + numbers))))
        (if (= 0 deviation) numbers
          (let* ((adjust (if (> 0 deviation) -1 1)) (numbers (list->vector numbers)))
            (let loop ((deviation (abs deviation)) (index (random count)))
              (let (adjusted (+ adjust (vector-ref numbers index)))
                (if (> minimum adjusted) (loop deviation (random count))
                  (begin (vector-set! numbers index adjusted)
                    (if (= 1 deviation) (vector->list numbers)
                      (loop (- deviation 1) (random count))))))))))))

  (define (euclidean-distance p0 p1)
    "(number ...) (number ...) -> number
     unlimited dimensions"
    (sqrt (apply + (map (l (p0d p1d) (expt (- p0d p1d) 2)) p0 p1))))

  (define (bezier-curve t . points)
    "number:0..1 (number ...) ... -> (number ...)
     get a point for a bezier curve at fractional offset t.
     no limit on the number of control points.
     no limit on the dimension of point vectors.
     at least one point must be given.
     uses the \"de casteljau\" algorithm"
    (if (null? (tail points)) (first points)
      (let ((left t) (right (- 1 t)))
        ; length of points is reduced by one for each recursive call
        (apply bezier-curve t
          ; use of pair-fold to check for following elements
          (pair-fold-right
            (l (a result)
              ; ignore last point
              (if (null? (tail a)) result
                (let ((p1 (first a)) (p2 (second a)))
                  (pair
                    (map
                      ; map all point dimensions
                      (l (p1d p2d) (+ (* right p1d) (* left p2d))) p1 p2)
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

  (define* (catmull-rom-interpolate-f p0 p1 p2 p3 #:optional (alpha 0.5) (tension 0))
    "(number ...) (number ...) (number ...) (number ...) [real:0..1 real:0..1] -> procedure:{real:0..1 -> (number ...)}
     return a function that gives points on a catmull-rom spline segment between p1 and p2 at fractional offsets.
     the returned function is called as (f time) where time is a real number between 0 and 1.
     to draw paths this function can be called with overlapping segments of a points series
     * alpha: 0 uniform, 0.5 centripetal, 1 chordal
     * tension: 0: smooth, 1: linear
     * no limit on the number of point dimensions
     adapted from code from mika rantanen at https://qroph.github.io/2018/07/30/smooth-paths-using-catmull-rom-splines.html"
    ; some coefficients constant for the segment are pre-calculated
    (let*
      ( (+ float-sum) (t01 (expt (euclidean-distance p0 p1) alpha))
        (t12 (expt (euclidean-distance p1 p2) alpha)) (t23 (expt (euclidean-distance p2 p3) alpha))
        (m1
          (map
            (l (p0d p1d p2d)
              (* (- 1 tension)
                (+ (- p2d p1d) (* t12 (- (/ (- p1d p0d) t01) (/ (- p2d p0d) (+ t01 t12)))))))
            p0 p1 p2))
        (m2
          (map
            (l (p1d p2d p3d)
              (* (- 1 tension)
                (+ (- p2d p1d) (* t12 (- (/ (- p3d p2d) t23) (/ (- p3d p1d) (+ t12 t23)))))))
            p1 p2 p3))
        (a (map (l (p1d p2d m1d m2d) (+ (* 2 (- p1d p2d)) m1d m2d)) p1 p2 m1 m2))
        (b (map (l (p1d p2d m1d m2d) (- (* -3 (- p1d p2d)) m1d m1d m2d)) p1 p2 m1 m2)) (c m1) (d p1))
      (l (t) (map (l (a b c d) (+ (* a t t t) (* b t t) (* c t) d)) a b c d))))

  (define (catmull-rom-path alpha tension resolution points)
    "real real integer ((number ...):point ...) -> ((number ...):point ...)
     create a smooth interpolated path from intermediate points of any but equal dimension.
     example: (catmull-rom-path 0.5 0 100 (quote ((-0.72 -0.3) (0 0) (1 0.8) (1.1 0.5) (2.7 1.2) (3.4 0.27))))"
    (let
      (points
        ; add one before and one after the series to interpolate between all given points,
        ; because the interpolation is only always between two of four points
        (let
          ( (first-point (map (l (p0d p1d) (- (* 2 p0d) p1d)) (first points) (second points)))
            (last-point
              (map (l (p0d p1d) (- (* 2 p0d) p1d)) (first (reverse points))
                (second (reverse points)))))
          (append (list first-point) points (list last-point))))
      (apply append
        (map-segments 4
          (l (p0 p1 p2 p3) "map points to point lists"
            (let (interpolate-f (catmull-rom-interpolate-f p0 p1 p2 p3 alpha tension))
              (map-integers resolution (l (t) (interpolate-f (/ t resolution))))))
          points))))

  (define (hermite-interpolate n tension bias p1 p2 p3 p4)
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

  (define (vector-linearly-interpolate offset a b)
    "real:0..1 (number ...) ... -> point
     return a point on a straight line between a and b at fractional offset"
    (vector-map (l (a b) (+ (* b offset) (* a (- 1 offset)))) a b))

  (define (linearly-interpolate offset a b)
    "real:0..1 (number ...) ... -> point
     return a point on a straight line between a and b at fractional offset"
    (map (l (a b) (+ (* b offset) (* a (- 1 offset)))) a b))

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

  (define (angle-between p1 p2)
    "#(number number) #(number number) -> number
     only for two dimensions"
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
            (vector-linearly-interpolate sub-n p1 p2))))))

  (define* (elliptical-arc n p1 p2 rx ry #:optional (rotation 0) large-arc sweep)
    "number:0..1 vector vector number number number:radians boolean boolean -> (vector . extra-calculated-values)
     extra-calculated-values: ((symbol:start-angle/end-angle/angle/center/radius-x/radius-y number) ...)
     return a point on an elliptical arc at fractional offset n.
     only for two dimensions.
     modeled after the svg path arc command.
     code translated from https://github.com/MadLittleMods/svg-curve-lib"
    ; there seems to be a bug in the scheme version with the sweep-angle
    (cond
      ((equal? p1 p2) p1)
      ((or (= 0 rx) (= 0 ry)) (vector-linearly-interpolate n p1 p2))
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
              end-angle (+ start-angle sweep-angle) angle angle center center radius-x rx radius-y ry))))))

  (define spline-path-new
    (let
      ( (line-new
          (l (segments rest-config . points) "list list list -> (segments next-start)"
            (let*
              ( (new-segments
                  (map-segments 2
                    (l (p0 p1)
                      (let* ((start (first p0)) (end (first p1)) (size (- end start)))
                        (vector p0 p1
                          ; segment-f
                          (l (offset) (linearly-interpolate (/ (- offset start) size) p0 p1)))))
                    points)))
              (list (append segments new-segments) (last points)))))
        (bezier-new
          (l (segments rest-config . points)
            (let*
              ( (p0 (first points)) (p1 (last points)) (start (first p0))
                (end (first p1)) (size (- end start))
                (new-segments
                  (list
                    (vector p0 p1
                      (l (offset) (apply bezier-curve (/ (- offset start) size) points))))))
              (list (append segments new-segments) (last points)))))
        (catmull-rom-new
          (l (segments rest-config p0 tension . points)
            (let*
              ( (points (pair p0 points))
                (points*
                  ; add points because cr interpolates only between p1 p2
                  (let*
                    ( (config-get-first-point
                        (l (a)
                          (case (first a)
                            ((catmull-rom) (third a))
                            (else (second a)))))
                      (following-points (map config-get-first-point rest-config))
                      (first-point
                        (if (> 2 (length segments))
                          (map (l (p0d p1d) (- (* 2 p0d) p1d)) (first points) (second points))
                          (spline-path-segment-end (second (reverse segments)))))
                      (last-point
                        (if (null? following-points)
                          (map (l (p0d p1d) (- (* 2 p0d) p1d)) (first (reverse points))
                            (second (reverse points)))
                          (first following-points))))
                    (append (list first-point) points (list last-point))))
                (new-segments
                  (map-segments 4
                    (l (p0 p1 p2 p3)
                      (let*
                        ( (start (first p1)) (end (first p2)) (size (- end start))
                          (interpolate-f (catmull-rom-interpolate-f p0 p1 p2 p3 0.5 tension)))
                        (vector p1 p2 (l (offset) (interpolate-f (/ (- offset start) size))))))
                    points*)))
              (list (append segments new-segments) (last points)))))
        (move-new (l (segments rest-config p0 p1) (list segments p1)))
        (arc-new
          (l (segments rest-config p0 p1 . params)
            "the arc ends at point (x, y)
            the ellipse has the two radii (rx, ry)
            the x-axis of the ellipse is rotated by x-axis-rotation"
            ; hasnt been tested in a while
            (apply
              (l* (radius-x #:optional (radius-y radius-x) (rotation 0) large-arc sweep)
                (let*
                  ( (start (first p0)) (end (first p1)) (size (- end start))
                    (p0-vector (list->vector p0)) (p1-vector (list->vector p1))
                    (new-segments
                      (list
                        (vector p0 p1
                          (l (offset)
                            (vector->list
                              (first
                                (elliptical-arc (/ (- offset start) size) p0-vector
                                  p1-vector radius-x radius-y rotation large-arc sweep))))))))
                  (list (append segments new-segments) p1)))
              params)))
        (infer-dimensions (l (segments) (length (second (first segments))))))
      (l (segments-config)
        "number ((symbol:type number:offset any:parameter ...) ...) -> path-state
         * the returned object is to be passed to spline-path to get points on a path between
           given points interpolated by selected functions
         * all interpolation functions can draw paths in an unlimited number of dimensions except for arc, which is 2d only
         * the dimensions of all segments must be the same
         * there are no required segment types but at least one must be given
         * if no \"move\" segment is given as the first element then the path starts at zero
         * move can also be used as a path segment to create gaps
         * this implementation is similar to the path element of svg vector graphics
         * for \"arc\" see how arcs are created with a path with svg graphics
         * the catmull-rom interpolation is always centripetal
         * the given segments describe the endpoints as in \"line to\" or \"move to\"
         * point: (number:dimension ...)
         # segment types
         * (move point)
         * (line point ...)
         * (bezier point ...)
         * (catmull-rom tension:0..1 point ...)
         * (arc (x y) radius-x [radius-y rotation large-arc sweep])
         # example
         (spline-path-new* (move (20 0)) (line (50 0.25)) (line (80 0.4)) (line (120 0.01)))"
        (let*
          ( (dimensions (infer-dimensions segments-config))
            (segments
              ; map segment configuration to segment objects with functions that draw it.
              ; pair-fold allows to pass following segment-config to handlers
              (first
                (pair-fold-multiple
                  (l (a segments start)
                    (let ((current (first a)) (rest (tail a)))
                      (apply
                        (case (first current)
                          ((line) line-new)
                          ((bezier) bezier-new)
                          ((catmull-rom) catmull-rom-new)
                          ((arc) arc-new)
                          ((move) move-new)
                          (else (raise (q spline-path-unknown-segment-type))))
                        segments rest start (tail current))))
                  segments-config null (make-list dimensions 0))))
            (next (first segments)) (index-i 0) (index (apply vector segments)))
          (list next index-i index dimensions)))))

  (define-syntax-rule (spline-path-new* segment ...) (spline-path-new (list (quote segment) ...)))

  (define (spline-path-advance path-state)
    "path-state -> path-state
     continue with the next segment"
    (apply
      (l (next index-i index dimensions)
        (let (index-i (+ 1 index-i))
          (if (< index-i (vector-length index))
            (list (vector-ref index index-i) index-i index dimensions) (list #f #f index dimensions))))
      path-state))

  (define spline-path-segment-start (vector-accessor 0))
  (define spline-path-segment-end (vector-accessor 1))
  (define spline-path-segment-f (vector-accessor 2))

  (define* (spline-path time path #:optional (c pair))
    "number path-state [procedure -> result] -> (data . path-state)/any
     get value at time for a path created by spline-path-new.
     returns a zero vector for gaps or when after the end of the path.
     the returned path-state does not have to be passed to subsequent calls to
     spline-path but it can be more efficient to do so,
     unless repeated access to previously returned points is required"
    (let (a (first path))
      (if (and a (>= time (first (spline-path-segment-start a))))
        (if (< time (first (spline-path-segment-end a)))
          (pair ((spline-path-segment-f a) time) path)
          (spline-path time (spline-path-advance path) c))
        (pair (make-list (last path) 0) path)))))
