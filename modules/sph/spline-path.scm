(library (sph spline-path)
  (export
    spline-path
    spline-path->procedure
    spline-path-append
    spline-path-constant
    spline-path-copy
    spline-path-dimensions
    spline-path-end
    spline-path-map-segments
    spline-path-new
    spline-path-new*
    spline-path-null
    spline-path-shift
    spline-path-start
    spline-path?)
  (import
    (rnrs exceptions)
    (sph)
    (sph math)
    (sph number)
    (sph vector)
    (only (guile) make-list inf)
    (only (sph list) map-segments pair-fold-multiple)
    (only (srfi srfi-1) find-tail))

  (define sph-math-spline-path-description "paths between interpolated points")
  (define spline-path-start (vector-accessor 1))
  (define spline-path-end (vector-accessor 2))
  (define spline-path-current-start (vector-accessor 3))
  (define spline-path-current-end (vector-accessor 4))
  (define spline-path-current-f (vector-accessor 5))
  (define spline-path-rest (vector-accessor 6))
  (define spline-path-all (vector-accessor 7))
  (define spline-path-dimensions (vector-accessor 8))
  (define spline-path-null (vector-accessor 9))
  (define spline-path-start-set! (vector-setter 1))
  (define spline-path-end-set! (vector-setter 2))
  (define spline-path-current-start-set! (vector-setter 3))
  (define spline-path-current-end-set! (vector-setter 4))
  (define spline-path-current-f-set! (vector-setter 5))
  (define spline-path-rest-set! (vector-setter 6))
  (define spline-path-all-set! (vector-setter 7))
  (define spline-path-segment-start (vector-accessor 0))
  (define spline-path-segment-end (vector-accessor 1))
  (define spline-path-segment-f (vector-accessor 2))
  (define spline-path-copy vector-copy)

  (define spline-path-new
    (let
      ( (line-new
          (l (segments rest-config . points)
            "list list list -> (segment:#(rel-start rel-end f) ...)
            segment-f are called with times relative to the segment start as t(zero).
            this allows segment start/ends to be modified without affecting segment-f"
            (map-segments 2
              (l (p0 p1)
                (vector p0 p1
                  (let*
                    ( (start (first p0)) (end (first p1)) (size (- end start))
                      (p0 (pair 0 (tail p0))) (p1 (pair (- end start) (tail p1))))
                    (l (t) (linearly-interpolate (/ t size) p0 p1)))))
              points)))
        (bezier-new
          (l (segments rest-config . points)
            (let*
              ( (last-point (last points)) (first-point (first points)) (start (first first-point))
                (size (- (first last-point) start)))
              (list
                (vector first-point last-point
                  (let (points (map (l (a) (pair (- (first a) start) (tail a))) points))
                    (l (t) (apply bezier-curve (/ t size) points))))))))
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
                    (append (list first-point) points (list last-point)))))
              (map-segments 4
                (l (p0 p1 p2 p3)
                  (let*
                    ( (start (first p1)) (end (first p2)) (size (- end start))
                      (interpolate-f
                        (let
                          ( (p0 (pair (- (first p0) start) (tail p0))) (p1 (pair 0 (tail p1)))
                            (p2 (pair (- end start) (tail p2)))
                            (p3 (pair (- (first p3) start) (tail p3))))
                          (catmull-rom-interpolate-f p0 p1 p2 p3 0.5 tension))))
                    (vector p1 p2 (l (t) (interpolate-f (/ t size))))))
                points*))))
        (move-new
          (l (segments rest-config p0 p1)
            (let ((end (first p1)) (null-point (make-list (- (length p1) 1) 0)))
              (list (vector p0 p1 (l (t) (if (= t end) p1 (pair t null-point))))))))
        (constant-new
          (l* (segments rest-config previous #:optional (p0 previous))
            (let (p0-tail (tail p0))
              (list (vector p0 (pair (inf) p0-tail) (l (t) (pair t p0-tail)))))))
        (arc-new
          (l (segments rest-config p0 p1 . params)
            "the arc ends at point (x, y)
            the ellipse has the two radii (rx, ry)
            the x-axis of the ellipse is rotated by x-axis-rotation"
            (apply
              (l* (radius-x #:optional (radius-y radius-x) (rotation 0) large-arc sweep)
                (let*
                  ( (start (first p0)) (end (first p1)) (size (- end start))
                    (p0-vector (list->vector p0)) (p1-vector (list->vector p1)))
                  (list
                    (vector p0 p1
                      (l (offset)
                        (vector->list
                          (first
                            (elliptical-arc (/ (- offset start) size) p0-vector
                              p1-vector radius-x radius-y rotation large-arc sweep))))))))
              params)))
        (custom-simple-new
          (l (segments rest-config p0 p1 f . a)
            (list
              (vector p0 p1
                (let ((p0* (pair 0 (tail p0))) (p1* (pair (- (first p1) (first p0)) (tail p1))))
                  (l (t) (apply f t p0* p1* a)))))))
        (custom-new
          (l (segments rest-config p0 f . a) "-> list:new-segments"
            (apply f segments rest-config p0 a)))
        (infer-dimensions (l (segments) (length (second (first segments))))))
      (l (segments-config)
        "((symbol:interpolator-name any:parameter ...) ...) -> path
         the returned object is to be passed to spline-path to get points on a path between
         given points interpolated by selected functions. similar to the path element of svg vector graphics.
         points in the given segment configuration are relative to the start of the path.
         is a duration.
         # data types
         * point: (number:dimension ...)
         * segment: #(point:start point:end procedure:{number -> point})
         # dimensions
         * the number of dimensions must be equal between all segments
         * all interpolators support an unlimited number of dimensions except for arc, which is 2d only
         # segments
         * the given segments describe the endpoints as in \"line to\" or \"move to\"
         * at least one segment must be given
         # segment types
         ## syntax
         * (move point)
         * (line point ...)
         * (bezier point ...)
         * (catmull-rom tension:0..1 point ...)
         * (arc (x y) radius-x [radius-y rotation large-arc sweep])
         * (constant [point])
         * (custom-simple point:end f custom-arguments ...)
         * (custom f custom-arguments ...)
         ## move
         * move can be given as the first element to start the path at this point
         * move can also be used to create gaps
         ## constant
         * describes a flat line to t(infinity)
         * infinitely repeats the values of all dimensions except the first
         * the optional point argument is a move
         * cant be used inbetween other segments as the end is never reached
         ## custom
         takes a procedure and arguments passed with each call to the procedure.
         f :: preceeding-segments following-config start custom-argument ... -> ((segment ...) next-start)
         f :: (vector ...) (list ...) point any ... -> (vector list)
         segment: #(point:start point:end procedure:{t -> point})
         # other
         * the catmull-rom interpolation is always centripetal
         * custom-simple takes a procedure (t offset start:point end:point any:custom ... -> point)
         * for \"arc\" see how arcs are created with svg
         # example
         (spline-path-new* (move (20 0)) (line (20 0.25)) (line (10 0.4)) (line (30 0.01)))"
        (let*
          ( (dimensions (infer-dimensions segments-config))
            (segments
              ; map segment configuration to segment objects with functions that draw it.
              ; pair-fold allows to pass following segment-config to handlers
              (first
                (pair-fold-multiple
                  (l (a segments start)
                    (let*
                      ( (current (first a)) (rest (tail a))
                        (segment-new
                          (case (first current)
                            ((line) line-new)
                            ((catmull-rom) catmull-rom-new)
                            ((arc) arc-new)
                            ((bezier) bezier-new)
                            ((constant) constant-new)
                            ((custom) custom-new)
                            ((custom-simple) custom-simple-new)
                            ((move) move-new)
                            (else (raise (q spline-path-unknown-segment-type)))))
                        (new-segments (apply segment-new segments rest start (tail current))))
                      (if (null? new-segments) (list segments start)
                        (list (append segments new-segments)
                          (spline-path-segment-end (last new-segments))))))
                  segments-config null (make-list dimensions 0))))
            (current (first segments)) (current-start (first (spline-path-segment-start current))))
          ;(debug-log segments)
          (vector (q spline-path)
            ; path-start
            current-start
            ; path-end
            (first (spline-path-segment-end (last segments)))
            ; current-start
            current-start
            ; current-end
            (first (spline-path-segment-end current))
            ; current-f
            (spline-path-segment-f current)
            ; rest
            (tail segments)
            ; all
            segments
            ; dimensions
            dimensions
            ; null
            (make-list (- dimensions 1) 0))))))

  (define-syntax-rule (spline-path-new* segment ...)
    (spline-path-new (list (quasiquote segment) ...)))

  (define (spline-path-set-rest a rest)
    (let (b (first rest)) (spline-path-rest-set! a rest)
      (spline-path-current-start-set! a (first (spline-path-segment-start b)))
      (spline-path-current-end-set! a (first (spline-path-segment-end b)))
      (spline-path-current-f-set! a (spline-path-segment-f b)))
    a)

  (define (spline-path-forward time a)
    (spline-path-set-rest a
      (find-tail (l (a) (<= time (first (spline-path-segment-end a)))) (spline-path-rest a)))
    a)

  (define (spline-path time path)
    "number path -> (time number ...):point
     get value at time for a path created by spline-path-new.
     returns a zero vector for gaps and before or after the path.
     path may get modified, use spline-path-copy to create paths that work in parallel"
    (if (and (>= time (spline-path-start path)) (<= time (spline-path-end path)))
      (let*
        ( (path
            (if (>= time (spline-path-current-start path))
              (if (<= time (spline-path-current-end path)) path (spline-path-forward time path))
              (spline-path-forward time (spline-path-set-rest path (spline-path-all path)))))
          (start (spline-path-current-start path))
          (point ((spline-path-current-f path) (- time start))))
        (pair (+ start (first point)) (tail point)))
      (pair time (spline-path-null path))))

  (define (spline-path->procedure path) (l (t) (spline-path t path)))

  (define (spline-path-constant . a)
    "create a path that always returns the same values. the first time dimension is still updated.
     equivalent to (sp-path-new* (constant (0 a ...)))"
    (spline-path-new (list-qq (constant (0 (unquote-splicing a))))))

  (define (spline-path? a) "any -> boolean"
    (and (vector? a) (not (= 0 (vector-length a))) (eq? (q spline-path) (vector-first a))))

  (define (spline-path-map-segments! a f)
    (let (all (map f (spline-path-all a)))
      (spline-path-start-set! a (first (spline-path-segment-start (first all))))
      (spline-path-end-set! a (first (spline-path-segment-end (last all))))
      (spline-path-all-set! a all) (spline-path-set-rest a all)))

  (define (spline-path-shift a amount)
    "spline-path number -> spline-path
     return a spline path that is the given path shifted in time by positive or negative amount"
    (let (a (spline-path-copy a))
      (spline-path-map-segments! a
        (l (a)
          (let ((start (spline-path-segment-start a)) (end (spline-path-segment-end a)))
            (vector (pair (+ amount (first start)) (tail start))
              (pair (+ amount (first end)) (tail end)) (spline-path-segment-f a)))))
      a))

  (define (spline-path-append a b)
    "spline-path ... -> spline-path
     return a new path that is a concatenation of the given paths"
    (let*
      ( (a (spline-path-copy a)) (a-end (spline-path-end a))
        (b-all
          (map
            (l (b) "move all b segments after path a"
              (let ((start (spline-path-segment-start b)) (end (spline-path-segment-end b)))
                (vector (pair (+ a-end (first start)) (tail start))
                  (pair (+ a-end (first end)) (tail end)) (spline-path-segment-f b))))
            (spline-path-all b)))
        (all (append (spline-path-all a) b-all)))
      (spline-path-end-set! a (+ a-end (spline-path-end b))) (spline-path-all-set! a all)
      (spline-path-set-rest a all))))
