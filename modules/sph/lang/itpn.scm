(library (sph lang itpn)
  (export
    itfpn-tags
    itfpn-tags-sort
    itpn-from-file
    itpn-from-port
    itpn-packets-sort
    itpn-prefixes
    itpn-string
    line->tags
    tags->line)
  (import
    (guile)
    (rnrs sorting)
    (sph)
    (sph lang indent-syntax)
    (sph list)
    (sph string)
    (sph tree)
    (sph two)
    (only (srfi srfi-1) partition))

  (define sph-lang-itpn-description
    "helpers for working with parsed itpn
     itpn: indent tree packet notation
     ittpn: indent tree tag packet notation")

  (define (line->tags a) (string-split a #\space))
  (define (tags->line a) (string-join a " "))
  (define (line-sort-tags a less?) (tags->line (list-sort less? (line->tags (first a)))))
  (define (itfpn-tags-sort a less?) (map (l (e) (pair (line-sort-tags e less?) (tail e))) a))
  (define (itpn-prefixes a) (map first a))
  (define (itpn-packets-sort a less?) (list-sort-with-accessor less? first a))

  (define (itfpn-tags a)
    "parsed-itpn -> (string ...)
     may include duplicates"
    (append-map (l (e) (line->tags (first e))) a))

  (define (itpn-from-port a) "-> parsed-itpn" (read-indent-tree->prefix-tree a))
  (define (itpn-from-file path) "-> parsed-itpn" (call-with-input-file path itpn-from-port))
  (define (itpn-string a) "parsed-itpn -> string" (prefix-tree->indent-tree a)))
