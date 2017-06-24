(library (sph lang itpn)
  (export
    itfpn-tags
    itfpn-tags-sort
    itpn-filter
    itpn-filter-all-patterns-all-parts
    itpn-filter-some-patterns-some-parts
    itpn-packets-sort
    itpn-prefixes
    line->tags
    tags->line)
  (import
    (guile)
    (rnrs sorting)
    (sph)
    (sph list)
    (sph one)
    (sph string)
    (sph tree)
    (sph two)
    (except (srfi srfi-1) map))

  (define sph-lang-itpn-description
    "helpers for working with parsed itpn
     itpn: indent tree packet notation
     ittpn: indent tree tag packet notation")

  (define
    (itpn-filter a patterns-prefix patterns-suffix patterns-anywhere string-contains-multiple?
      parts-combination)
    "parsed-itpn (string ...) (string ...) (string ...) procedure:{string (string ...) -> boolean} procedure:{any ... -> boolean/any:last} -> list:(filtered rejected)
     filters itpn elements where all patterns of a set match in the corresponding portion - prefix, suffix or anywhere.
     the empty set matches all"
    (apply-values list
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

  (define (line->tags a) (string-split a #\space))
  (define (tags->line a) (string-join a " "))
  (define (line-sort-tags a less?) (tags->line (list-sort less? (line->tags (first a)))))
  (define (itfpn-tags-sort a less?) (map (l (e) (pair (line-sort-tags e less?) (tail e))) a))
  (define (itpn-prefixes a) (map first a))
  (define (itpn-packets-sort a less?) (list-sort-with-accessor less? first a))

  (define (itfpn-tags a)
    "parsed-itpn -> (string ...)
     may include duplicates"
    (append-map (l (e) (line->tags (first e))) a)))
