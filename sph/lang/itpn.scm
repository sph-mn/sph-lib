(library (sph lang itpn)
  (export
    facets->line
    itfpn-facets-collect
    itfpn-facets-sort
    itpn-element-contains?
    itpn-filter
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

  ;indent tree packet notation

  (define (itpn-element-contains-all? element patterns where)
    (case where ((prefix) (string-contains-every? (first element) patterns))
      ((suffix) (any (l (e) (string-contains-every? e patterns)) (flatten (tail element))))
      (else
        (or (itpn-element-contains-all? element patterns (q prefix))
          (itpn-element-contains-all? element patterns (q suffix))))))

  (define (itpn-filter a patterns where)
    "parsed-itpn (string ...) symbol:both/suffix/prefix -> pair:(filtered rejected)
    filters itpn elements where all patterns match at the relevant portion - prefix, suffix or both"
    (apply-values pair (partition (l (e) (itpn-element-contains-all? e patterns where)) a)))

  (define (line->facets a) (string-split a #\space))
  (define (facets->line a) (string-join a " "))
  (define (line-sort-facets a less?) (facets->line (list-sort less? (line->facets (first a)))))
  (define (itfpn-facets-sort a less?) (map (l (e) (pair (line-sort-facets e less?) (tail e))) a))

  (define (itfpn-facets-collect a)
    (delete-duplicates (append-map (l (e) (line->facets (first e))) a))))