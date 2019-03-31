(library (sph spline-path)
  (export
    sph-spline-path-description
    spline-path
    spline-path->procedure
    spline-path-append
    spline-path-combine
    spline-path-compose-input-mapper
    spline-path-config
    spline-path-config->generic-config
    spline-path-config-distance->t
    spline-path-config-t->distance
    spline-path-constant
    spline-path-constant?
    spline-path-dimensions
    spline-path-end
    spline-path-infinite?
    spline-path-input-mapper
    spline-path-map-config
    spline-path-map-segments
    spline-path-modify
    spline-path-new
    spline-path-new*
    spline-path-new-generic
    spline-path-null
    spline-path-repeat
    spline-path-start
    spline-path?)
  (import
    (rnrs exceptions)
    (sph)
    (sph list other)
    (sph math)
    (sph number)
    (sph vector)
    (only (guile)
      compose
      inf
      make-list
      *random-state*)
    (only (sph list)
      fold-segments
      map-apply
      map-segments
      pair-fold-multiple)
    (only (srfi srfi-1) find take-right))

  (define sph-spline-path-description "interpolated paths through points")
  (define spline-path-start (vector-accessor 1))
  (define spline-path-end (vector-accessor 2))
  (define spline-path-index (vector-accessor 3))
  (define spline-path-dimensions (vector-accessor 4))
  (define spline-path-null (vector-accessor 5))
  (define spline-path-config (vector-accessor 6))
  (define spline-path-input-mapper (vector-accessor 7))
  (define spline-path-output-mapper (vector-accessor 8))
  (define spline-path-segment-start (vector-accessor 0))
  (define spline-path-segment-end (vector-accessor 1))
  (define spline-path-segment-f (vector-accessor 2))
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
        ; segment-index
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
     * (catmull-rom point point ...)
     * (catmull-rom-tension tension point point ...)
     * (arc (x y):point radius-x [radius-y rotation large-arc sweep])
     * (custom-simple point:end/(point ...) f custom-arguments ...)
     * (custom point/(point ...) f custom-arguments ...)
     ## move
     * move can be given as the first element to start the path at this point
     * move can also be used to create gaps
     ## constant
     * describes a flat line to t(infinity)
     * infinitely repeats the values of all dimensions except the first time dimension
     * the optional point argument is a move
     * not useful inbetween segments as the end is never reached
     ## custom
     takes a procedure and arguments passed with each call to the procedure.
     f :: preceeding-segments following-config points custom-argument ... -> (segment ...)
     f :: (vector ...) (list ...) (point ...) any ... -> (vector ...)
     segment: #(point:start point:end procedure:{t -> point})
     # mapping
     * input-mapper maps time when spline-path is called. f :: t -> t
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

  (define (spline-path-constant . a)
    "create a path that always returns the same values. the first time dimension is still updated.
     equivalent to (sp-path-new* (constant (0 a ...)))"
    (spline-path-new (list-qq (constant (0 (unquote-splicing a))))))

  (define (spline-path-infinite? a)
    "spline-path -> boolean
     true if the path has infinite length"
    (infinite? (spline-path-end a)))

  (define (spline-path-constant? a)
    "spline-path -> boolean
     true if path uses a single constant segment and the path never changes"
    (eq? (q constant) (first (first (spline-path-config a)))))

  (define (spline-path-current a time)
    (find (l (a) (<= time (first (spline-path-segment-end a)))) (spline-path-index a)))

  (define (spline-path time path)
    "number path -> (time number ...):point
     get value at time for a path created by spline-path-new.
     returns a zero vector for gaps and before or after the path."
    ( (spline-path-output-mapper path)
      (let (time ((spline-path-input-mapper path) time))
        (if (and (>= time (spline-path-start path)) (<= time (spline-path-end path)))
          (let*
            ( (current (spline-path-current path time))
              (start (first (spline-path-segment-start current)))
              (point ((spline-path-segment-f current) (- time start))))
            (pair (+ start (first point)) (tail point)))
          (pair time (spline-path-null path))))
      time))

  (define (spline-path? a) "any -> boolean"
    (and (vector? a) (= 9 (vector-length a)) (eq? (q spline-path) (vector-first a))))

  (define (spline-path->procedure a) "spline-path -> {number:t -> (t number ...)}"
    (if (spline-path-constant? a)
      (let (point (first (second (first (spline-path-config a))))) (l (t) (pair t (tail point))))
      (l (t) (spline-path t a))))

  (define (spline-path-config->generic-config a)
    "((type arguments ...) ...) -> ((type points other) ...)
     convert a spline-path-new config to the internally used more generic config where points
     and arguments are at predictable places"
    (let
      (result
        (map
          (l (a)
            (let ((type (first a)) (a (tail a)))
              (case type
                ((line bezier move catmull-rom constant) (list type a null))
                ((catmull-rom-tension) (list type (tail a) (list (first a))))
                ( (arc custom custom-simple)
                  (let (points (first a))
                    (list type (if (number? (first points)) (list points) points) (tail a))))
                (else (raise (q spline-path-unknown-segment-type))))))
          a))
      (case (first (first result))
        ((constant move) result)
        (else
          ; add implied start point
          (pair (list (q move) (list (make-list (length (first (second (first result)))) 0)) null)
            result)))))

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
                  (let
                    (segment-relative-points
                      (map (l (a) (pair (- (first a) start) (tail a))) points))
                    (l (t) (apply bezier-curve (/ t size) segment-relative-points))))))))
        (move-new
          (l (segments next points)
            (let*
              ((p1 (last points)) (end (first p1)) (null-point (make-list (- (length p1) 1) 0)))
              (list (vector (first points) p1 (l (t) (if (= t end) p1 (pair t null-point))))))))
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
          (l* (segments next points #:optional (tension 0))
            "points are at least two, (start end).
            the last point of the element of segments is the current start point"
            (let*
              ; add points to keep the tangents at the connecting points
              ( (points
                  (pair (map (l (p0d p1d) (- p0d (- p1d p0d))) (first points) (second points))
                    points))
                (points
                  (append points
                    (list
                      (let (last-two (take-right points 2))
                        (map (l (p0d p1d) (+ p1d (- p1d p0d))) (first last-two) (second last-two)))))))
              (map-segments 4
                (l (p0 p1 p2 p3)
                  ; make segment-relative
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
                          ((catmull-rom catmull-rom-tension) catmull-rom-new)
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

  (define (spline-path-compose-input-mapper a b)
    "procedure procedure -> procedure
     only compose the procedures if none is the default identity mapper"
    (if (eq? default-input-mapper b) a (if (eq? default-input-mapper a) b (compose a b))))

  (define (spline-path-compose-output-mapper a b)
    "procedure procedure -> procedure
     only compose the procedures if none is the default identity mapper"
    (if (eq? default-output-mapper b) a
      (if (eq? default-output-mapper a) b (l (point t) (a (b point t) t)))))

  (define (spline-path-map-config a f)
    "path procedure:{generic-config input-mapper -> (generic-config input-mapper output-mapper)} -> path
     create a new path with parameters mapped by f"
    ; by recreating the path it allows for all kinds of changes including dimension changes
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

  (define (spline-path-config-distance->t config)
    "generic-config -> generic-config
     convert the first point values which are point-relative distances to path-relative offsets"
    (let*
      ( (with-fold-arguments
          (l (f)
            "expects the fold state value to be a list and
           passes it as multiple arguments to f.
           only for versions of fold where the state is the first argument.
           example: if state is (1 2) and current fold arguments are 3 4 then call (f 1 2 3 4)"
            (l (result . a) (apply f (append result a)))))
        (points (map second config))
        (fold-result
          (fold-segments 2
            (with-fold-arguments
              (l (path-points path-time a b)
                "list integer (point ...) (point ...) -> ((point ...) integer)"
                (let (path-time (+ path-time (first (first b))))
                  (apply
                    (l (segment-points segment-time)
                      (list
                        (pair (pair (pair path-time (tail (first b))) (reverse segment-points))
                          path-points)
                        segment-time))
                    (if (null? (tail b)) (list null path-time)
                      (fold-segments 2
                        (with-fold-arguments
                          (l (segment-points time a b)
                            "list integer point point -> ((point ...) integer)"
                            (let (time (+ time (first b)))
                              (list (pair (pair time (tail b)) segment-points) time))))
                        (list null path-time) b))))))
            (list null (first (first (first points)))) points))
        (points (pair (first points) (reverse (first fold-result))))
        (config (map (l (a b) (list (first b) a (third b))) points config)))
      config))

  (define (spline-path-config-t->distance config)
    "generic-config -> generic-config
     convert the first point values which are path-relative offets to point-relative distances"
    (let*
      ( (points (map second config))
        (points
          (pair (first points)
            (map-segments 2
              (l (a b)
                "get distance between last of previous segment and current segment,
              and distances between points inside the segment"
                (pair (pair (- (first (first b)) (first (last a))) (tail (first b)))
                  (if (null? (tail b)) null
                    (map-segments 2 (l (a b) (pair (- (first b) (first a)) (tail b))) b))))
              points)))
        (config (map (l (a b) (list (first b) a (third b))) points config)))
      config))

  (define* (spline-path-config-randomise config #:optional random-state)
    "spline-path generic-config [random-state] -> generic-config
     randomise the order of segments. the starting point stays the same"
    (spline-path-config-distance->t
      (let (distance-config (spline-path-config-t->distance config))
        (pair (first distance-config)
          (randomise (tail distance-config)
            (if (boolean? random-state) *random-state* random-state))))))

  (define (spline-path-config-stretch config to-end)
    (let*
      ( (start (first (first (second (first config))))) (end (first (first (second (last config)))))
        (old-size (- end start)) (factor (/ (- to-end start) old-size)))
      (map-apply
        (l (type points other)
          (list type (map (l (a) (pair (* factor (first a)) (tail a))) points) other))
        config)))

  (define (spline-path-config-scale config factor)
    (map-apply
      (l (type points other)
        (list type (map (l (a) (pair (first a) (map (l (a) (* factor a)) (tail a)))) points) other))
      config))

  (define (spline-path-config-reverse config . a)
    (let*
      ( (start (first (first (second (first config))))) (end (first (first (second (last config)))))
        (end-values (tail (last (second (last config))))) (points (map second config))
        (points
          ; convert times
          (reverse
            (map
              (l (a)
                (reverse (map (l (point) (pair (+ start (- end (first point))) (tail point))) a)))
              points)))
        (points
          ; shift points left by one over sub-lists
          (pair (list (first (first points)))
            (map-segments 2 (l (a b) (append (tail a) (list (first b)))) points)))
        (config
          (map (l (a b) (list (first b) a (third b))) points
            (pair (first config) (reverse (tail config))))))
      config))

  (define (spline-path-config-shift config amount)
    "spline-path number -> spline-path
     return a new path that is the given path shifted in time by positive or negative amount"
    (map-apply
      (l (type points other)
        (list type (map (l (a) (pair (+ amount (first a)) (tail a))) points) other))
      config))

  (define*
    (spline-path-modify a #:key randomise repeat reverse scale shift stretch input-mapper
      output-mapper)
    "spline-path [keys ...] -> spline-path
     return a new path with specified modifications.
     # keys
     input-mapper: false/procedure
     output-mapper: false/procedure
     randomise: boolean/random-state
     reverse: boolean
     scale: false/number:factor
     shift: false/number:amount
     stretch: false/number:to-end
     repeat: boolean/number (experimental)
     # example
     (spline-path-modify path #:reverse #t #:randomise (random-state-from-platform)
       #:scale 0.2 #:stretch (* 2 (spline-path-end path)))"
    (spline-path-map-config a
      (l (config old-input-mapper old-output-mapper)
        (let
          (result
            (fold (l (mod config) (if (first mod) ((tail mod) config (first mod)) config)) config
              (list (pair randomise spline-path-config-randomise)
                (pair reverse spline-path-config-reverse)
                (pair (and scale (not (= 1 scale)) scale) spline-path-config-scale)
                (pair stretch spline-path-config-stretch)
                (pair (and shift (not (= 1 shift)) shift) spline-path-config-shift))))
          (if repeat
            ; this featuer is currently not well supported. mappers are overwritten
            ; and spline-path-end will give the wrong value
            (list result
              (spline-path-compose-input-mapper
                (let (end (first (first (second (last result)))))
                  (if (number? repeat) (l (t) (if (> t (* repeat end)) t (fmod t end)))
                    (l (t) (fmod t end))))
                (or input-mapper default-input-mapper))
              (spline-path-compose-output-mapper
                (l (point original-t) (pair original-t (tail point)))
                (or output-mapper default-output-mapper)))
            (list result (or input-mapper old-input-mapper) (or output-mapper old-output-mapper)))))))

  (define (spline-path-combine f . a)
    "return a new path that is a combination of the given paths.
     each point of path is combined by f for each dimension.
     example that sums three paths: (sp-path-combine + path1 path2 path3)"
    (spline-path-modify (first a) #:output-mapper
      (spline-path-compose-output-mapper
        (l (point t)
          (let (points (pair point (map (l (a) (spline-path t a)) (tail a))))
            (pair t (apply map f (map tail points)))))
        (spline-path-output-mapper (first a))))))
