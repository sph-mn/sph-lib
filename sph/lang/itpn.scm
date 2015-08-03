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
    (sph)
    (sph tree)
    (sph list)
    (rnrs sorting)
    (except (srfi srfi-1) map)
    (sph string))

  ;indent tree packet notation

  (define (itpn-element-contains? element patterns where)
    (case where ((prefix) (string-contains-any? (first element) patterns))
      ((suffix) (any (l (e) (string-contains-any? e patterns)) (flatten (tail element))))
      (else (or (contains? element patterns (q prefix)) (contains? element patterns (q suffix))))))

  (define (itpn-filter a patterns where)
    (filter (l (e) (itpn-element-contains? e patterns where)) a))

  (define (line->facets a) (string-split a #\space))
  (define (facets->line a) (string-join a " "))
  (define (line-sort-facets a less?) (facets->line (list-sort less? (line->facets (first a)))))
  (define (itfpn-facets-sort a less?) (map (l (e) (pair (line-sort-facets e less?) (tail e))) a))

  (define (itfpn-facets-collect a)
    (delete-duplicates (append-map (l (e) (line->facets (first e))) a))))