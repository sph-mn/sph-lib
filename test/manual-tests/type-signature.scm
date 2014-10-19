(import (sph) (sph lang parser type-signature))

(define str-sig-multiline
  "::
                  g [h i ...]
                  -> string/boolean:a/j b c

                  docu")

;(define str-sig-line "[a/b:c/d ... {a -> b}]")

(define str-sig-line
  "a {d -> e f} -> b
      c -> d g
    ")

#;(define str-sig-line "a -> b
  c -> d")

;(debug-log (peg:tree (match-pattern sig-multiline str-sig-multiline)))
;(define t "procedure:{current-interval procedure:continue:{next-interval -> any} -> any} microseconds -> unspecified")
;(define t "{current-interval procedure:continue:{next-interval -> any} -> any} microseconds -> unspecified")
;(define t "a:b:c -> d")
;(define t "{states ... -> integer} microseconds microseconds rational rational any ... ->")
;(define t "a (integer ...) -> b")
;(define t "a #(integer ...) -> b")
;(define t "list with one element -> element")
;(define t "a -> ((b . c) ...)")
;(define t "procedure:{keyword/any:cli-create-argument ...} ->")
;(define t "procedure:{d ->} -> a")
(define t
  "::
  procedure:{alist:header procedure:fold-lines:{string:line any:result procedure:next:{any:result ->} ->} result -> any}
  procedure:{header port result ->} any port [string]
  ->
  any")
;(define t "improper-list -> (list . non-pair)
;    (1 2 . 3) -> (1 2) 3")

(define t "symbol/hashtable -> test")
(debug-log (parsed-type-signature->string (debug-log (string->parsed-type-signature t))))

#;(debug-log
  (let (preparsed-sig (peg:tree (match-pattern sig str-sig-line)))
    (tree-map-lists
      (l (ele)
        (if (and (list? ele) (not (null? ele)))
          (case (first ele)
            ((sig-line-output) (list (q sig-line-output) (apply parse-arguments (tail ele))))
            (else ele))
          ele))
      preparsed-sig)))