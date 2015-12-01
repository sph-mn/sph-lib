(library (sph lang itpn)
  (export
    facets->line
    itfpn-facets
    itfpn-facets-sort
    itpn-filter
    itpn-filter-all-patterns-all-parts
    itpn-filter-some-patterns-some-parts
    itpn-packets-sort
    itpn-prefixes
    line->facets)
  (import
    (guile)
    (rnrs base)
    (rnrs sorting)
    (sph)
    (sph list)
    (sph one)
    (sph string)
    (sph tree)
    (except (srfi srfi-1) map))

  ;itpn: indent tree packet notation
  ;itfpn: indent tree facet packet notation

  (define
    (itpn-filter a patterns-prefix patterns-suffix patterns-anywhere string-contains-multiple?
      parts-combination)
    "parsed-itpn (string ...) (string ...) (string ...) procedure:{string (string ...) -> boolean} procedure:{any ... -> boolean/any:last} -> pair:(filtered rejected)
    filters itpn elements where all patterns of a set match in the corresponding portion - prefix, suffix or anywhere.
    the empty set matches all"
    (apply-values pair
      (partition
        (l (e)
          (let (e (if (string? e) (list e) e))
            (parts-combination
              (or (null? patterns-prefix) (string-contains-multiple? (first e) patterns-prefix))
              (or (null? patterns-suffix)
                (any (l (e) (string-contains-multiple? e patterns-suffix)) (flatten (tail e))))
              (or (null? patterns-anywhere)
                (any (l (e) (string-contains-multiple? e patterns-anywhere)) (flatten e))))))
        a)))

  (define (itpn-filter-all-patterns-all-parts a patterns-prefix patterns-suffix patterns-anywhere)
    "list list list -> (list list)
    filters packets where all patterns-prefix match the prefix and all patterns-suffix match the suffix"
    (itpn-filter a patterns-prefix patterns-suffix patterns-anywhere string-contains-every? and-p))

  (define
    (itpn-filter-some-patterns-some-parts a patterns-prefix patterns-suffix patterns-anywhere)
    "list list list -> (list list)
    filters packets where some patterns-prefix match the prefix or some patterns-suffix match the suffix"
    (itpn-filter a patterns-prefix patterns-suffix patterns-anywhere string-contains-any? or-p))

  (define (line->facets a) (string-split a #\space))
  (define (facets->line a) (string-join a " "))
  (define (line-sort-facets a less?) (facets->line (list-sort less? (line->facets (first a)))))
  (define (itfpn-facets-sort a less?) (map (l (e) (pair (line-sort-facets e less?) (tail e))) a))
  (define (itpn-prefixes a) (map first a))
  (define (itpn-packets-sort a less?) (list-sort-with-accessor less? first a))

  (define (itfpn-facets a)
    "parsed-itpn -> (string ...)
    may include duplicates"
    (append-map (l (e) (line->facets (first e))) a)))