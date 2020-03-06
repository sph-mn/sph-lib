(define-module (sph spline-path))

(use-modules (sph) (sph list other)
  (sph math) (sph number)
  (sph vector) ((rnrs base) #:select (infinite?))
  ((sph alist) #:select (alist-delete-multiple alist-merge))
  ( (sph list) #:select
    (any->list compact fold-segments fold* map-apply map-segments pair-fold-multiple))
  (srfi srfi-1))

(export sph-spline-path-description spline-path
  spline-path->procedure spline-path->procedure-fast
  spline-path-append spline-path-combine
  spline-path-config spline-path-config->generic-config
  spline-path-config-distance->t spline-path-config-t->distance
  spline-path-constant spline-path-constant?
  spline-path-end spline-path-fast
  spline-path-infinite? spline-path-map-config
  spline-path-map-segments spline-path-modify
  spline-path-new spline-path-new*
  spline-path-new-generic spline-path-null spline-path-repeat spline-path-start spline-path?)

(define sph-spline-path-description
  "interpolated paths through points.
   spline-path-new creates path objects and spline-path gets values from it.
   path objects contain a configuration object that can be modified to create a new modified path.
   paths can also contain other paths as segments.
   # usage
   ~~~
   (define path-all
     (spline-path-new*
       (move (2 10))
       (line (10 20) (20 50))
       (bezier (30 10) (40 40) (50 10))
       (catmull-rom (60 10) (70 40) (80 10) (90 50))
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
   ~~~
   spline-path gets points on the path at offset:
   ~~~
   (spline-path 0 path-all)
   (spline-path 1 path-all)
   (spline-path 55 path-all)
   ~~~")

(define spline-path-start (vector-accessor 1))
(define spline-path-end (vector-accessor 2))
(define spline-path-index (vector-accessor 3))
(define spline-path-null (vector-accessor 4))
(define spline-path-config (vector-accessor 5))
(define spline-path-mapper-config (vector-accessor 6))
(define spline-path-input-mapper (vector-accessor 7))
(define spline-path-output-mapper (vector-accessor 8))
(define spline-path-segment-start (vector-accessor 0))
(define spline-path-segment-end (vector-accessor 1))
(define spline-path-segment-f (vector-accessor 2))

(define* (spline-path-new-generic config #:optional mapper)
  "list [list] -> spline-path
   new spline path object from generic-config.
   spline-path always gives a point with the time value to output mapper that was originally passed to spline-path,
   so that input mapper can change it and the output will have the users requested time.
   having mappers in a separate config makes internally used mappers easier to replace and subsequently added user mappers manageable"
  (let*
    ( (dimensions (length (first (second (first config))))) (null-point (make-list dimensions 0))
      (segments (spline-path-generic-config->segments config null-point)) (current (first segments))
      (current-start (first (spline-path-segment-start current))) (mapper (or mapper null)))
    (vector (q spline-path) current-start
      (first (spline-path-segment-end (last segments))) segments
      (tail null-point) config mapper (filter-map second mapper) (filter-map third mapper))))

(define* (spline-path-new config #:optional mapper)
  "((symbol:interpolator-name any:parameter ...) ...) [(list ...)] -> path
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
   * (path spline-path ...)
   ## move
   * move can be given as the first element to start the path at this point
   * move can also be used to create gaps
   ## constant
   * describes a flat line to t(infinity)
   * infinitely repeats the values of all dimensions except the first time dimension
   * the optional point argument is a move
   * not useful inbetween segments as the next segment is never reached
   ## path
   * segments can be paths
   * this can be used to append multiple paths
   ## custom
   * calls a procedure that should return segments
   * additional config arguments are passed on
   * segment: #(point:start point:end procedure:{t -> point})
   * f :: preceeding-segments following-config points custom-argument ... -> (segment ...)
   * f :: (vector ...) (list ...) (point ...) any ... -> (vector ...)
   # mapping
   * an arbitrary number of procedures can be installed that are called either for the input to spline-path or the output
   * this allows for interesting transformations like dynamic time stretches, value scaling or path combination
   * the mapper support allows paths to be sampled using spline-path
   * the mapper argument is a list with mapper config elements
   * mapper config elements are (symbol:key false/procedure:{t -> t}:input false/procedure:{point -> point}:output any:custom-info ...)
   * key is a custom symbol except \"repeat\", which is used internally.
   * input and output mapper procedures are optional
   * the collection of all active mappers will be accessible in the path object with spline-path-mapper-config.
   * the active mapper list will be created from the mapper config
   # other
   * the catmull-rom interpolation is always centripetal
   * custom-simple takes a procedure (t points any:custom ... -> point)
   * for \"arc\" see how arcs are created with svg
   # example
   (spline-path-new* (move (20 0)) (line (20 0.25) (10 0.4)))"
  (spline-path-new-generic (spline-path-config->generic-config config) mapper))

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

(define (spline-path-fast time path)
  "bypasses all mappers and only works if time is on path.
   about 10% faster"
  (let*
    ((current (spline-path-current path time)) (start (first (spline-path-segment-start current))))
    (pair time (tail ((spline-path-segment-f current) (- time start))))))

(define (spline-path time path)
  "number path -> (time number ...):point
   get value at time for a path created by spline-path-new.
   returns a zero vector for gaps and before or after the path"
  (let*
    ( (input-mapper (spline-path-input-mapper path))
      (output-mapper (spline-path-output-mapper path))
      (t (if (null? input-mapper) time (fold (l (a t) (a t)) time input-mapper)))
      (point
        (pair time
          (if (and (>= t (spline-path-start path)) (<= t (spline-path-end path)))
            (let*
              ( (current (spline-path-current path t))
                (start (first (spline-path-segment-start current)))
                (point ((spline-path-segment-f current) (- t start))))
              (tail point))
            (spline-path-null path)))))
    (if (null? output-mapper) point (fold (l (a p) (a p)) point output-mapper))))

(define (spline-path? a) "any -> boolean"
  (and (vector? a) (= 9 (vector-length a)) (eq? (q spline-path) (vector-first a))))

(define (spline-path->procedure a) "spline-path -> {number:t -> (t number ...)}"
  (if (and #f (spline-path-constant? a) (null? (spline-path-mapper-config a)))
    (let (point (tail (first (second (first (spline-path-config a)))))) (l (t) (pair t point)))
    (l (t) (spline-path t a))))

(define (spline-path->procedure-fast a)
  "spline-path -> {number:t -> (t number ...)}
   uses spline-path-fast"
  (l (t) (spline-path-fast t a)))

(define (spline-paths->points paths start-time)
  "(spline-path ...) point -> (point:end ...)
   get a list of end points for one or a series of spline-paths.
   the time value is cumulated as if the paths were concatenated.
   paths start at zero or start-time "
  (let (last-points (map (compose last second last spline-path-config) paths))
    (reverse
      (first
        (fold*
          (l (a points time)
            (let (time (+ time (first a))) (list (pair (pair time (tail a)) points) time)))
          last-points null start-time)))))

(define (spline-path-config->generic-config a)
  "((type arguments ...) ...) -> ((type points other) ...)
   convert a spline-path-new config to the internally used more generic config where points
   and arguments are at predictable places"
  (let*
    ( (result
        (fold
          (l (a result) "use fold to have access to previous values"
            (let ((type (first a)) (a (tail a)))
              (pair
                (case type
                  ((line bezier move catmull-rom constant) (list type a null))
                  ((catmull-rom-tension) (list type (tail a) (list (first a))))
                  ( (arc custom custom-simple)
                    (let (points (first a))
                      (list type (if (number? (first points)) (list points) points) (tail a))))
                  ( (path)
                    (list type
                      (spline-paths->points a
                        (if (null? result) 0 (first (last (second (first result))))))
                      a))
                  (else (raise (q spline-path-unknown-segment-type))))
                result)))
          null a))
      (result (reverse result)))
    (case (first (first result))
      ((constant move) result)
      (else
        (let (dimensions (length (first (second (first result)))))
          (pair (list (q move) (list (make-list dimensions 0)) null) result))))))

(define spline-path-generic-config->segments
  (let
    ( (line-new
        (l (segments next points)
          "list list list -> (segment:#(point:start point:end procedure:interpolator any:custom ...)
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
                  (segment-relative-points (map (l (a) (pair (- (first a) start) (tail a))) points))
                  (l (t) (apply bezier-curve (/ t size) segment-relative-points))))))))
      (path-new
        (l (segments next points . paths)
          (map
            (l (path start-end)
              (vector (first start-end) (tail start-end) (l (t) (spline-path t path))))
            paths (map-segments 2 pair points))))
      (move-new
        (l (segments next points)
          (let* ((p1 (last points)) (end (first p1)) (null-point (make-list (- (length p1) 1) 0)))
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
        (l (segments next points f . a) "-> (vector ...):segments" (apply f segments next points a)))
      (catmull-rom-new
        (l* (segments next points #:optional (tension 0))
          "points are at least two, (start end).\n            the last point of segments is the current start point"
          (let*
            ( (points
                (pair (map (l (p0d p1d) (- p0d (- p1d p0d))) (first points) (second points)) points))
              (points
                (append points
                  (list
                    (let (last-two (take-right points 2))
                      (map (l (p0d p1d) (+ p1d (- p1d p0d))) (first last-two) (second last-two)))))))
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
          "the arc ends at point (x, y)\n            the ellipse has the two radii (rx, ry)\n            the x-axis of the ellipse is rotated by x-axis-rotation"
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
      "(list ...) (0 ...) -> (vector ...)
       map generic configuration to segment objects from which the current interpolator will be selected from.
       pair-fold allows to pass subsequent segment-config to handlers"
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
                        ((path) path-new)
                        (else (raise (q spline-path-unknown-segment-type))))
                      result rest (pair start points) other))
                  (if (null? segments) (list result start)
                    (list (append result segments) (spline-path-segment-end (last segments))))))
              (tail a) (first a)))
          a null null-point)))))

(define (spline-path-map-config a f)
  "path procedure:{generic-config mapper-config procedure:return -> spline-path} -> spline-path
   return :: generic-config mapper-config -> spline-path
   create a new path with parameters mapped by f.
   example: (spline-path-map-config path (lambda (config mapper c) (c config mapper)))"
  (f (spline-path-config a) (spline-path-mapper-config a) spline-path-new-generic))

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

(define (spline-path-config-map-sub-paths config f)
  "generic-config {spline-path -> spline-path} -> config
   map over the spline-paths in path segments"
  (reverse
    (fold
      (l (config-element result)
        (apply
          (l (type points other)
            (case type
              ( (path)
                (let (other (map f other))
                  (pair
                    (list type
                      (spline-paths->points other
                        (if (null? result) 0 (first (last (second (first result))))))
                      other)
                    result)))
              (else (pair config-element result))))
          config-element))
      null config)))

(define (spline-path-config-stretch config to-end)
  "contract or extend the path so it ends at to-end. nested path segments are processed as well"
  (let*
    ( (start (first (first (second (first config))))) (end (first (first (second (last config)))))
      (old-size (- end start)) (factor (/ (- to-end start) old-size)))
    (let loop ((config config))
      (map-apply
        (l (type points other)
          (list type (map (l (a) (pair (* factor (first a)) (tail a))) points)
            (case type
              ( (path)
                (map
                  (l (a) (spline-path-map-config a (l (config mapper c) (c (loop config) mapper))))
                  other))
              (else other))))
        config))))

(define (spline-path-config-scale config factor)
  (map-apply
    (l (type points other)
      (list type (map (l (a) (pair (first a) (map (l (a) (* factor a)) (tail a)))) points) other))
    config))

(define (spline-path-config-randomise config deep random-state)
  "spline-path generic-config [random-state] -> generic-config
   randomise the order of segments. the starting point stays the same"
  (let
    (config
      (spline-path-config-distance->t
        (let (distance-config (spline-path-config-t->distance config))
          (pair (first distance-config)
            (randomise (tail distance-config)
              (if (boolean? random-state) *random-state* random-state))))))
    (if deep
      (spline-path-config-map-sub-paths config
        (l (path)
          (spline-path-map-config path
            (l (config mapper c) (c (spline-path-config-randomise config deep random-state) mapper)))))
      config)))

(define (spline-path-config-reverse config deep)
  (let*
    ( (start (first (first (second (first config))))) (end (first (first (second (last config)))))
      (end-values (tail (last (second (last config))))) (points (map second config))
      (points
        (reverse
          (map
            (l (a)
              (reverse (map (l (point) (pair (+ start (- end (first point))) (tail point))) a)))
            points)))
      (points
        (pair (list (first (first points)))
          (map-segments 2 (l (a b) (append (tail a) (list (first b)))) points)))
      (config
        (map (l (a b) (list (first b) a (third b))) points
          (pair (first config) (reverse (tail config))))))
    (if deep
      (spline-path-config-map-sub-paths config
        (l (path)
          (spline-path-map-config path
            (l (config mapper c) (c (spline-path-config-reverse config deep) mapper)))))
      config)))

(define (spline-path-config-shift config amount)
  "spline-path number -> spline-path
   return a new path that is the given path shifted in time by positive or negative amount"
  (map-apply
    (l (type points other)
      (list type (map (l (a) (pair (+ amount (first a)) (tail a))) points) other))
    config))

(define*
  (spline-path-modify a #:key deep randomise repeat reverse scale shift stretch mapper-add
    mapper-remove)
  "spline-path [keys ...] -> spline-path
   return a new path with specified modifications.
   # keys
   randomise: boolean/random-state
   repeat: boolean/number (experimental)
   reverse: boolean
   scale: false/number:factor
   shift: false/number:amount
   stretch: false/number:to-end
   deep: with reverse or randomise: apply to paths as segments as well
   mapper-add: ((symbol:id false/procedure:input false/procedure:output custom-info ...) ...)
   mapper-remove: id/(id ...)
   # example
   (spline-path-modify path #:reverse #t #:randomise (random-state-from-platform)
     #:scale 0.2 #:stretch (* 2 (spline-path-end path)))"
  (spline-path-map-config a
    (l (config mapper c)
      (let
        ( (config
            (fold (l (mod config) (if (first mod) ((tail mod) config (first mod)) config)) config
              (list
                (pair randomise (l (config . a) (apply spline-path-config-randomise config deep a)))
                (pair reverse (l (config . a) (spline-path-config-reverse config deep)))
                (pair (and scale (not (= 1 scale)) scale) spline-path-config-scale)
                (pair stretch spline-path-config-stretch)
                (pair (and shift (not (= 1 shift)) shift) spline-path-config-shift))))
          (mapper
            (alist-merge
              (if mapper-remove (apply alist-delete-multiple mapper (any->list mapper-remove))
                mapper)
              (append (or mapper-add null)
                (if repeat
                  (list
                    (list (q repeat)
                      (let (end (first (first (second (last config)))))
                        (if (number? repeat) (l (t) (if (> t (* repeat end)) t (fmod t end)))
                          (l (t) (fmod t end))))
                      #f repeat))
                  null)))))
        (c config mapper)))))

(define (spline-path-append a . b)
  "spline-path ... -> spline-path
   add one or more spline-paths as a path segment to the end of path a"
  (spline-path-map-config a
    (l (config mapper c)
      (c
        (append config
          (list (q path) (list (spline-path-start (first b)) (spline-path-end (last b))) b))
        mapper))))

(define (spline-path-combine f a . b)
  "return a new path that is a combination of the given paths.
   each point of path is combined by f for each dimension.
   example that sums three paths: (spline-path-combine + path1 path2 path3)"
  (spline-path-map-config a
    (l (config mapper c)
      (c config
        (pair
          (list (q combination) #f
            (l (a-point)
              (let*
                ((t (first a-point)) (all-points (pair a-point (map (l (b) (spline-path t b)) b))))
                (pair t (apply map f (map tail all-points))))))
          mapper)))))
