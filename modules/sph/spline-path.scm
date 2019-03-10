(library (sph spline-path)
  (export
    sph-spline-path-description
    spline-path
    spline-path->procedure
    spline-path-append
    spline-path-compose-input-mapper
    spline-path-compose-output-mapper
    spline-path-config
    spline-path-config->generic-config
    spline-path-constant
    spline-path-copy
    spline-path-dimensions
    spline-path-end
    spline-path-map-segments
    spline-path-modify
    spline-path-new
    spline-path-new*
    spline-path-new-generic
    spline-path-null
    spline-path-shift
    spline-path-start
    spline-path-stretch
    spline-path?)
  (import
    (rnrs exceptions)
    (sph)
    (sph math)
    (sph number)
    (sph vector)
    (only (guile)
      compose
      make-list
      inf)
    (only (sph list)
      map-apply
      map-segments
      pair-fold-multiple)
    (only (srfi srfi-1) find-tail take-right))

  (define sph-spline-path-description "interpolated paths through points")
  ; accessors
  (define spline-path-start (vector-accessor 1))
  (define spline-path-end (vector-accessor 2))
  (define spline-path-current-start (vector-accessor 3))
  (define spline-path-current-end (vector-accessor 4))
  (define spline-path-current-f (vector-accessor 5))
  (define spline-path-rest (vector-accessor 6))
  (define spline-path-all (vector-accessor 7))
  (define spline-path-dimensions (vector-accessor 8))
  (define spline-path-null (vector-accessor 9))
  (define spline-path-config (vector-accessor 10))
  (define spline-path-input-mapper (vector-accessor 11))
  (define spline-path-output-mapper (vector-accessor 12))
  (define spline-path-segment-start (vector-accessor 0))
  (define spline-path-segment-end (vector-accessor 1))
  (define spline-path-segment-f (vector-accessor 2))
  ; setters
  (define spline-path-start-set! (vector-setter 1))
  (define spline-path-end-set! (vector-setter 2))
  (define spline-path-current-start-set! (vector-setter 3))
  (define spline-path-current-end-set! (vector-setter 4))
  (define spline-path-current-f-set! (vector-setter 5))
  (define spline-path-rest-set! (vector-setter 6))
  (define spline-path-all-set! (vector-setter 7))
  (define spline-path-config-set! (vector-setter 10))
  (define spline-path-input-mapper-set! (vector-setter 11))
  (define spline-path-output-mapper-set! (vector-setter 12))
  (define (spline-path-copy a) (vector-copy a))
  (define default-input-mapper identity)
  (define default-output-mapper (l (point t) point))

  (define* (spline-path-new-generic config #:optional input-mapper output-mapper)
    "new spline path object from generic-config"
    (let*
      ( (dimensions (length (first (second (first config))))) (null-point (make-list dimensions 0))
        (segments (spline-path-generic-config->segments config null-point))
        (current (first segments)) (current-start (first (spline-path-segment-start current))))
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
        (tail null-point)
        ; config
        config
        ; input-mapper
        (or input-mapper default-input-mapper)
        ; output-mapper
        (or output-mapper default-output-mapper))))

  (define* (spline-path-new config #:optional input-mapper output-mapper)
    "((symbol:interpolator-name any:parameter ...) ...) [procedure:{t -> t}] -> path
     the returned object is to be passed to spline-path to get points on a path between
     given points interpolated by selected functions. similar to the path element of svg vector graphics.
     points in the given segment configuration are relative to the start of the path.
     point: (number:dimension ...)
     # dimensions
     * the number of dimensions must be equal between all segments
     * all interpolators support an unlimited number of dimensions except for arc, which is 2d only
     # segments
     * the given segments describe the endpoints as in \"line to\" or \"move to\"
     * at least one segment must be given
     # segment types
     ## syntax
     * (move point)
     * (line point point ...)
     * (bezier point ...)
     * (catmull-rom tension:0..1 point point ...)
     * (arc (x y):point radius-x [radius-y rotation large-arc sweep])
     * (constant [point])
     * (custom-simple point:end/(point ...) f custom-arguments ...)
     * (custom point/(point ...) f custom-arguments ...)
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
     f :: preceeding-segments following-config points custom-argument ... -> (segment ...)
     f :: (vector ...) (list ...) (point ...) any ... -> (vector ...)
     segment: #(point:start point:end procedure:{t -> point})
     # mapping
     * input-mapper maps time when spline-path is called. f :: t -> t
     * output-mapper maps the result of spline-path. f :: point t -> point
     * allows for interesting transformations like dynamic time stretches, value scaling or path combination
     * the mapper support allows paths to still be sampled generically with spline-path
     # other
     * the catmull-rom interpolation is always centripetal
     * custom-simple takes a procedure (t points any:custom ... -> point)
     * for \"arc\" see how arcs are created with svg
     # example
     (spline-path-new* (move (20 0)) (line (20 0.25) (10 0.4)))"
    (spline-path-new-generic (spline-path-config->generic-config config) input-mapper output-mapper))

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
    ( (spline-path-output-mapper path)
      (let (time ((spline-path-input-mapper path) time))
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
      time))

  (define (spline-path-constant . a)
    "create a path that always returns the same values. the first time dimension is still updated.
     equivalent to (sp-path-new* (constant (0 a ...)))"
    (spline-path-new (list-qq (constant (0 (unquote-splicing a))))))

  (define (spline-path? a) "any -> boolean"
    (and (vector? a) (not (= 0 (vector-length a))) (eq? (q spline-path) (vector-first a))))

  (define (spline-path->procedure path) (l (t) (spline-path t path)))

  (define (spline-path-config->generic-config a)
    "((type arguments ...) ...) -> ((type points other) ...)
     convert a spline-path-new config to the internally used more generic config"
    (map
      (l (a)
        (let ((type (first a)) (a (tail a)))
          (case type
            ((line bezier move constant) (list type a null))
            ((catmull-rom) (list type (tail a) (list (first a))))
            ( (arc custom custom-simple)
              (let (points (first a))
                (list type (if (number? (first points)) (list points) points) (tail a))))
            (else (raise (q spline-path-unknown-segment-type))))))
      a))

  (define spline-path-generic-config->segments
    (let
      ( (line-new
          (l (segments next points)
            "list list list -> (segment:#(point:start point:end interpolator ...)
            interpolators are called with times relative to the segment start as t(zero).
            this allows segment start/ends to be modified without having to update interpolators"
            (map-segments 2
              (l (p0 p1)
                (vector p0 p1
                  (let*
                    ( (start (first p0)) (end (first p1)) (size (- end start))
                      (p0 (pair 0 (tail p0))) (p1 (pair (- end start) (tail p1))))
                    (l (t) (linearly-interpolate (/ t size) p0 p1)))))
              points)))
        (bezier-new
          (l (segments next points)
            (let*
              ( (last-point (last points)) (first-point (first points)) (start (first first-point))
                (size (- (first last-point) start)))
              (list
                (vector first-point last-point
                  (let (points (map (l (a) (pair (- (first a) start) (tail a))) points))
                    (l (t) (apply bezier-curve (/ t size) points))))))))
        (move-new
          (l (segments next points)
            (let*
              ( (p0 (first points)) (p1 (last points)) (end (first p1))
                (null-point (make-list (- (length p1) 1) 0)))
              (list (vector p0 p1 (l (t) (if (= t end) p1 (pair t null-point))))))))
        (constant-new
          (l* (segments next points)
            (let* ((p0 (last points)) (p0-tail (tail p0)))
              (list (vector p0 (pair (inf) p0-tail) (l (t) (pair t p0-tail)))))))
        (custom-simple-new
          (l (segments next points f . a)
            (let*
              ( (p0 (first points)) (p1 (last points)) (start (first p0))
                (points
                  (pair (pair 0 (tail p0))
                    (map (l (a) (pair (- (first a) start) (tail a))) (tail points)))))
              (list (vector p0 p1 (l (t) (apply f t points a)))))))
        (custom-new
          (l (segments next points f . a) "-> (vector ...):segments"
            (apply f segments next points a)))
        (catmull-rom-new
          (l (segments next points tension)
            ; points are at least two, (start end).
            ; the last point of the element of segments is the current start point.
            ; add points because cr interpolates only between the middle two points
            (let*
              ( (points
                  (pair
                    (if (null? segments)
                      (map (l (p0d p1d) (- p0d (- p1d p0d))) (first points) (second points))
                      (spline-path-segment-start (last segments)))
                    points))
                (points
                  (append points
                    (if (null? next)
                      (let (last-two (take-right points 2))
                        (list
                          (map (l (p0d p1d) (- p0d (- p1d p0d))) (first last-two) (second last-two))))
                      (list (first (second (first next))))))))
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
                points))))
        (arc-new
          (l*
            (segments next points
              radius-x #:optional (radius-y radius-x) (rotation 0) large-arc sweep)
            "the arc ends at point (x, y)
            the ellipse has the two radii (rx, ry)
            the x-axis of the ellipse is rotated by x-axis-rotation"
            (let*
              ( (p0 (first points)) (p1 (last points)) (start (first p0))
                (end (first p1)) (size (- end start))
                (p0-vector (list->vector p0)) (p1-vector (list->vector p1)))
              (list
                (vector p0 p1
                  (l (t)
                    (vector->list
                      (first
                        (elliptical-arc (/ (- t start) size) p0-vector
                          p1-vector radius-x radius-y rotation large-arc sweep))))))))))
      (l (a null-point)
        "(list ...) -> (vector ...)
         map generic configuration to segment objects from which the current interpolator will be selected from.
         pair-fold allows to pass following segment-config to handlers"
        (first
          (pair-fold-multiple
            (l (a result start)
              (apply
                (l (rest type points other)
                  (let
                    (segments
                      (apply
                        (case type
                          ((line) line-new)
                          ((catmull-rom) catmull-rom-new)
                          ((arc) arc-new)
                          ((bezier) bezier-new)
                          ((constant) constant-new)
                          ((custom) custom-new)
                          ((custom-simple) custom-simple-new)
                          ((move) move-new)
                          (else (raise (q spline-path-unknown-segment-type))))
                        result rest (pair start points) other))
                    (if (null? segments) (list result start)
                      (list (append result segments) (spline-path-segment-end (last segments))))))
                (tail a) (first a)))
            a null null-point)))))

  (define (spline-path-shift a amount)
    "spline-path number -> spline-path
     return a new path that is the given path shifted in time by positive or negative amount"
    (spline-path-new-generic
      (map
        (l (type points other)
          (list type (map (l (a) (pair (+ amount (first a)) (tail a))) points) other))
        (spline-path-config a))
      (spline-path-input-mapper a) (spline-path-output-mapper a)))

  (define (spline-path-compose-input-mapper a b)
    "procedure procedure -> procedure
     only compose the procedures if none is the default identity mapper"
    (if (eq? default-input-mapper b) a (if (eq? default-input-mapper a) b (compose a b))))

  (define (spline-path-compose-output-mapper a b)
    (if (eq? default-output-mapper b) a (if (eq? default-output-mapper a) b (compose a b))))

  (define (spline-path-modify a f)
    "path procedure:{generic-config input-mapper output-mapper -> (generic-config input-mapper output-mapper)} -> path
     create a new path with parameters mapped by f"
    ; allows for all kinds of changes including dimension changes
    (apply spline-path-new-generic
      (f (spline-path-config a) (spline-path-input-mapper a) (spline-path-output-mapper a))))

  (define (spline-path-append a b)
    "spline-path ... -> spline-path
     return a new path that is a concatenation of the given paths"
    (spline-path-new-generic
      (append (spline-path-config a)
        (map-apply
          (let (a-end (spline-path-end a))
            (l (type points other)
              (list type (map (l (a) (pair (+ a-end (first a)) (tail a))) points) other)))
          (spline-path-config b)))
      (spline-path-compose-input-mapper (spline-path-input-mapper a) (spline-path-input-mapper b))
      (spline-path-compose-output-mapper (spline-path-output-mapper a)
        (spline-path-output-mapper b))))

  (define (spline-path-stretch a amount)
    "return a new path that is a version of the given path compressed or elongated in time by factor"
    (let (a (spline-path-copy a))
      (spline-path-input-mapper-set! a
        (spline-path-compose-input-mapper (l (t) (/ t amount)) (spline-path-input-mapper a)))
      (spline-path-output-mapper-set! a
        (spline-path-compose-output-mapper (l (point t) (pair t (tail point)))
          (spline-path-output-mapper a)))
      a)))
