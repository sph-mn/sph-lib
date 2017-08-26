; Copyright (C) 2010-2017 sph <sph@posteo.eu>
; This program is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.

(library (sph list)
  (export
    any->list
    any->list-s
    compact
    complement
    complement-both
    consecutive
    contains-all?
    contains-any?
    contains?
    containsq?
    containsv-any?
    containsv?
    count-value
    count-value-with-limit
    count-with-limit
    define-list
    delete-duplicates-sorted
    difference
    difference+intersection
    difference+intersection-p
    difference-p
    drop*
    each-first-middle-last
    each-in-index-range
    each-slice
    each-with-index
    every-map
    every-or-index
    false-if-null
    filter-append-map
    filter-not
    filter-produce
    first-if-single
    first-intersection
    first-intersection-p
    first-or-false
    first-or-null
    flat?
    flatten
    fold-integers
    fold-multiple
    fold-multiple-right
    fold-multiple-with-continue
    fold-segments
    fold-slice
    fold-span
    fold-unless
    fold-unless-check-init
    fold-until
    fold-with-buffer
    group-consecutive
    group-split-at-matches
    improper-list-split-at-last
    insert-second
    integer->list
    interleave
    intersection
    intersection-p
    iterate-three
    iterate-three-with-stop+end
    length-greater-one?
    length-one?
    list-bind
    list-distribute
    list-distribute-sorted
    list-index-value
    list-indices
    list-page
    list-prefix?
    list-q
    list-qq
    list-replace-last
    list-replace-last-n
    list-select
    list-set-equal?
    list-set-eqv?
    list-set-match-condition?
    list-set-match-contains?
    list-sort-by-list
    list-sort-with-accessor
    list-suffix?
    map-apply
    map-consecutive
    map-first
    map-integers
    map-map
    map-one
    map-segments
    map-selected
    map-slice
    map-span
    map-unless
    map-with-continue
    map-with-index
    map-with-state
    pair->list
    pair-bind
    pair-fold-multiple
    pair-map
    pair-reverse
    pattern-match-min-length
    produce
    produce-controlled
    produce-unless
    replace
    replace-at-once
    replace-value
    simplify
    simplify-list
    splice
    splice-last-list
    split-at-last
    split-at-value
    split-by-pattern
    tail-or-null
    take*
    true->list
    true->list-s
    union
    (rename (lset-adjoin list-set-add)
      (list-tail list-tail-ref)
      (lset-difference complement-p)
      (lset<= list-set-subset?)))
  (import
    (ice-9 match)
    (sph)
    (srfi srfi-31)
    (except (rnrs base) map)
    (except (srfi srfi-1) map)
    (only (guile)
      filter
      inf
      identity
      map
      memv
      memq
      member
      negate)
    (only (rnrs sorting) list-sort))

  ;this library also contains bindings for non-list pairs. either create a new library or rename this one to (sph pair).

  (define-syntax-rule (identity-if test else ...)
    ;copied from (sph conditional)
    ((lambda (r) (if r r (begin else ...))) test))

  (define-syntax-rule (list-q a ...) (q (a ...)))
  (define-syntax-rule (list-qq a ...) (qq (a ...)))

  (define-syntax-rule (list-bind a lambda-formals body ...)
    ;bind elements of list "a" to "lambda-formals"
    (apply (lambda lambda-formals body ...) a))

  (define-syntax-rule (any->list-s a)
    ;"like \"any->list\" but as syntax"
    (if (list? a) a (list a)))

  (define-syntax-rule (true->list-s a)
    ;"like \"any->list-s\" but results in \"a\" if \"a\" is not true"
    (if a (any->list-s a) a))

  (define-syntax-rule (define-list name a ...) (define name (list a ...)))

  (define-syntax-rule (pair-bind a (b c) body ...)
    ;binds the first and second value of "a" to "b" and "c" respectively. ideally, maybe, lambda/apply should support (apply (lambda (a . b)) (pair 1 2))
    ((lambda (b c) body ...) (first a) (tail a)))

  (define (drop* count a)
    "like srfi-1 drop but with reversed argument order (like stream-drop from srfi-41) and
    returns null if list contains less elements than count instead of raising an exception"
    (if (<= (length a) count) (list) (drop a count)))

  (define (take* count a)
    "like srfi-1 take but with reversed argument order (like stream-take from srfi-41) and
    returns null if list contains less elements than count instead of raising an exception"
    (if (<= (length a) count) a (take a count)))

  (define (list-page a entry-count number lookahead c)
    "list integer integer integer procedure:{list boolean:last-page? -> any} -> any
     pass a list of \"entry-count\" elements at an offset of (* number entry-count),
     eventually including \"lookahead\" number of elements if they are the last elements,
     and a boolean indicating if it is the last page to continuation procedure \"c\""
    (let*
      ( (offset (* (- number 1) entry-count)) (rest (if (< 1 number) (drop* offset a) a))
        (lookahead-entries (take* lookahead (drop* entry-count rest)))
        (page (take* entry-count rest)))
      (if (= lookahead (length lookahead-entries)) (c page #f)
        (c (append page lookahead-entries) #t))))

  (define (flatten a)
    "list -> (non-list ...)
     replace sublists with their content, resulting in a list that does not contain lists"
    (fold-right (l (e r) (if (list? e) (append (flatten e) r) (pair e r))) (list) a))

  (define (append-map-unless proc stop? default . a)
    "procedure:{any:list-element ... -> any} procedure:{any -> boolean} any list ... -> list/false
     map unless \"stop?\" is true for a map result. result in \"default\" if \"stop?\" is true"
    (if (any null? a) (list)
      (let loop ((rest (map tail a)) (e (apply proc (map first a))) (init (list)))
        (if (stop? e) default
          (if (any null? rest) (append init e)
            (loop (map tail rest) (apply proc (map first rest)) (append init e)))))))

  (define (non-empty-list? a)
    "any -> boolean
     true if argument is a list with at least one element"
    (and (list? a) (not (null? a))))

  (define (every-or-index proc . a)
    "procedure:{any ... -> boolean} list ... -> true/integer
     true if \"proc\" is true for all elements, otherwise the index of the element for which \"proc\" failed"
    (let (r (apply list-index (negate (l e (apply proc e))) a)) (if (boolean? r) (not r) r)))

  (define (fold-every proc init . a)
    "{any any -> any} any list ... -> any
     like fold, but every result must be a \"true\" value, otherwise the result is false"
    (if init (if (null? a) init (fold-every proc (proc (map first a) init) (map tail a))) init))

  (define (any->list a)
    "any -> list
     wraps a non-list argument in a list"
    (any->list-s a))

  (define (true->list a)
    "any -> list/false
     wraps a true non-list argument in a list"
    (true->list-s a))

  (define (map-first proc a)
    "procedure list -> list
     call \"proc\" for the first element of list and replace the first element in the list with the result of \"proc\".
     replace-first"
    (pair (proc (first a)) (tail a)))

  (define (replace-at-once match? proc a)
    "procedure:{any -> boolean} procedure:{list:matched-elements -> list:replacements} list:source -> list
     all elements matching \"match?\" are collected in a list and passed to \"proc\".
     the result of \"proc\" is then used to replace the matched elements in source in order"
    (reverse
      (first
        (let (a-extended (map (l (e) (pair (match? e) e)) a))
          (fold-multiple
            (l (e r replacements)
              (if (first e) (list (pair (first replacements) r) (tail replacements))
                (list (pair (tail e) r) replacements)))
            a-extended (list) (proc (filter-map (l (e) (if (first e) (tail e) #f)) a-extended)))))))

  (define (contains-all? a values)
    "list ... -> boolean
     test if argument \"a\" contains all of the given values"
    (every (l (b) (contains? a b)) values))

  (define (contains-any? a values)
    "list ... -> boolean
     test if argument \"a\" contains any of the given values"
    (any (l (b) (contains? a b)) values))

  (define (containsv-any? a values)
    "list ... -> boolean
     test if argument \"a\" contains any of the given values"
    (any (l (b) (containsv? a b)) values))

  (define* (contains? a value #:optional (member member))
    "list any [procedure:{any list -> boolean/any}] -> boolean
     return a boolean indicating if list \"a\" contains \"value\""
    (if (member value a) #t #f))

  (define (containsq? a value) (if (memq value a) #t #f))
  (define (containsv? a value) (if (memv value a) #t #f))

  (define* (count-value value a #:optional (equal? equal?))
    "any list -> integer
     count occurences of \"value\" in list"
    (fold (l (a r) (if (equal? value a) (+ 1 r) r)) 0 a))

  (define* (count-value-with-limit value a #:optional (count-limit (inf)) (member member))
    "any list [integer procedure:{any list -> boolean/any}] -> integer
     like count-value but with an optional parameter for a count at which to stop counting"
    (let loop ((rest (member value a)) (count 0))
      (if (pair? rest)
        (let (count (+ count 1))
          (if (= count-limit count) count (loop (member value (tail rest)) count)))
        count)))

  (define* (count-with-limit pred limit . a)
    "procedure integer list ... -> integer
     like \"count\" but with an optional parameter for a count at which to stop counting"
    (let loop ((rest a) (count 0))
      (if (any null? rest) count
        (if (apply pred (map first rest))
          (let (count (+ 1 count)) (if (= count limit) count (loop (map tail rest) count)))
          (loop (map tail rest))))))

  (define (complement-both a b)
    "list list -> (list list)
     delete elements in both lists that are included in both lists"
    (list (complement a b) (complement b a)))

  (define (complement . lists)
    "list ... -> list
     delete elements from the first list that are included in the other lists"
    (apply lset-difference equal? lists))

  (define (union . a) (delete-duplicates (apply append a)))

  (define* (delete-duplicates-sorted a #:optional (equal-proc equal?) (preserve-order #t))
    "list [procedure:{any any -> boolean} boolean] -> list
     delete duplicates from a sorted list using a more efficient algorithm than for unsorted lists"
    (if (or (null? a) (null? (tail a))) a
      ( (if preserve-order reverse (l (i) i))
        (let loop ((e-1 (first a)) (rest (tail a)) (r (list (first a))))
          (if (null? rest) r
            (let ((e-2 (first rest)))
              (loop e-2 (tail rest) (if (equal-proc e-1 e-2) r (pair e-2 r)))))))))

  (define (each-slice proc slice-length a)
    "procedure:{list ->} integer list ->
     apply proc to each slice of slice-length elements from list."
    (let loop ((rest a) (cur-slice (list)) (cur-length 0))
      (if (null? rest) (if (not (null? cur-slice)) (proc cur-slice))
        (if (= slice-length cur-length)
          (begin (proc cur-slice) (loop (tail rest) (list (first rest)) 1))
          (loop (tail rest) (pair (first rest) cur-slice) (+ 1 cur-length))))))

  (define (each-with-index proc . a)
    "procedure:{index element ... ->} list ... ->
     apply proc to each element and its index in list a"
    (let loop ((rest a) (index 0))
      (if (not (any null? rest))
        (begin (apply proc index (map first rest)) (loop (map tail rest) (+ index 1))))))

  (define (each-in-index-range proc start end . a)
    "procedure integer integer list ... ->
     untested.
     call proc only for elements in index range between \"start\" and \"end\" inclusively"
    (let loop ((rest a) (index (if (< end 0) (+ end (length (first a))) 0)))
      (if (<= index end)
        (begin (if (>= index start) (apply proc (map first rest))) (loop (map tail rest))))))

  (define (each-first-middle-last first-proc middle-proc last-proc . a)
    "procedure procedure procedure list ... ->
     untested.
     call \"first-proc\" for the first element,
     call \"last-proc\" for the last element,
     call \"middle-proc\" for a list of all elements inbetween"
    (apply first-proc (map first a))
    (let loop ((rest (map tail a)) (count (- (length (first a)) 1)))
      (if (= 0 count) (apply last-proc (map first rest))
        (begin (middle-proc (map first rest)) (loop (map tail rest) (- count 1))))))

  (define (every-map proc . a)
    "procedure:{any -> any} list ... -> list/false
     like map but results in false if any result of proc is not a true value"
    (apply map-unless proc not #f a))

  (define (compact a)
    "list -> list
     keep only true elements in list. removes all boolean false values"
    (filter identity a))

  (define (filter-append-map proc . lists) "apply filter-map and then apply append on the result"
    (apply append (apply filter-map proc lists)))

  (define (difference-p equal-proc . lists)
    "{any any -> boolean} list ... -> list
     like"
    difference " but the predicate for comparing list elements can be specified"
    (let (every* (l (a b) (every (l (e-2) (any (l (e-3) (equal-proc a e-3)) e-2)) b)))
      (iterate-three
        (l (head e rest . r)
          (append (filter (l (e) (not (and (every* e rest) (every* e head)))) e) r))
        lists)))

  (define (difference . lists)
    "list ... -> list
     result in a list of elements not included in all given lists"
    (apply difference-p equal? lists))

  (define (difference+intersection-p equal-proc . lists)
    "{any any -> boolean} list ... -> (list:difference list:intersection)
     like difference+intersection but the predicate for comparing list elements can be specified"
    (let (every* (l (a b) (every (l (e-2) (any (l (e-3) (equal-proc a e-3)) e-2)) b)))
      (iterate-three
        (l (head e rest d i)
          (fold-multiple
            (l (e d i)
              (if (and (every* e rest) (every* e head)) (list d (pair e i)) (list (pair e d) i)))
            e d i))
        lists (list) (list))))

  (define (difference+intersection . lists)
    "list ... -> (list list)
     results in a list with two elements, one being the symmetric-difference between the given lists and one being the intersection.
     that means one list of all elements that are included in all lists, and one list of elements that are not included in all lists.
     it does both calculations in one step saving resources compared to making them in separate steps."
    (apply difference+intersection-p equal? lists))

  (define (intersection-p equal-proc . rest)
    "procedure:{any any -> boolean} list ... -> list
     like \"intersection\" but the predicate for comparing the list elements can be specified"
    (if (any null? rest) (list)
      (filter (l (e) (every (l (e-2) (any (l (e-3) (equal-proc e e-3)) e-2)) (tail rest)))
        (first rest))))

  (define (intersection . lists)
    "list ... -> list
     result in a list of all elements which are contained in all given lists"
    (apply intersection-p equal? lists))

  (define (filter-not proc a)
    "{any -> boolean} list -> list
     like filter but keeps only values where the result of proc is false.
     same as (filter (compose not proc) list)"
    (filter (l (e) (not (proc e))) a))

  (define (filter-produce proc . a)
    "procedure list ...
     apply \"proc\" to each ordered combination of elements from lists, cartesian product, and return true results in a list.
     can handle multiple lists and non-list arguments.
     example:
     (produce proc (1 2) (4 5) (6))
     is equivalent to
     (list (proc 1 4 6) (proc 1 5 6) (proc 2 4 6) (proc 2 5 6))"
    (let loop ((rest (map any->list a)) (args (list)))
      (if (null? rest) (apply proc args)
        (let (tail-rest (tail rest))
          ( (if (null? tail-rest) filter-map filter-append-map)
            (l e (loop tail-rest (append args e))) (first rest))))))

  (define (first-intersection-p equal-proc a b)
    "{any any -> boolean} list list -> any
     like first-intersection but the procedure for comparing elements can be specified"
    (find (l (b) (any (l (a) (equal-proc a b)) a)) b))

  (define (first-intersection a b)
    "list list -> any
     give the first found element that is included in both lists"
    (first-intersection-p equal? a b))

  (define (first-if-single a)
    "list -> any/list
     give the first element of a list if the list has only one element, otherwise give the list"
    (if (or (null? a) (not (null? (tail a)))) a (first a)))

  (define (first-or-false a)
    "list -> any/false
     give the first element of a list if it is not null, otherwise false"
    (if (null? a) #f (first a)))

  (define (false-if-null a) (if (null? a) #f a))

  (define (first-or-null a)
    "results in the first element of a list if it is not null, otherwise null"
    (if (null? a) a (first a)))

  (define (flat? a)
    "list -> boolean
     true if the list does not contain a list"
    (if (null? a) #t (if (list? (first a)) #f (flat? (tail a)))))

  (define (fold-span filter-proc proc a)
    "procedure:{any -> any/false} procedure:{list -> any} list -> any
     fold over each list of elements that consecutively matched filter-proc (utilising the \"span\" procedure)"
    (let loop ((rest a) (r (list)))
      (if (null? rest) (reverse r)
        (let-values (((consecutive rest) (span filter-proc rest)))
          (if (null? consecutive) (loop (tail rest) (pair (first rest) r))
            (loop rest (proc consecutive r)))))))

  (define (fold-multiple proc a . custom-state-values)
    "procedure:{any:list-element any:state-value ... -> (any:state-value)} list any:state-value ... -> list:state-values
     like fold but with multiple state values. the state values are updated by returning a list from a call to \"proc\".
     apply \"proc\" to each element of \"a\" and the state-value elements that were given to
     fold-multiple or subsequently the updated state-values from the previous call to \"proc\""
    (if (null? a) custom-state-values
      (apply fold-multiple proc (tail a) (apply proc (first a) custom-state-values))))

  (define (fold-multiple-with-continue proc a . custom-state-values)
    "procedure:{any:element procedure:continue:{list:next-pair any:state-value ...} any:state-value ... -> any} list any:state-value ... -> list"
    (if (null? a) custom-state-values
      (apply proc (first a)
        (l custom-state-values
          (apply fold-multiple-with-continue proc (tail a) custom-state-values))
        custom-state-values)))

  (define (fold-multiple-right proc a . r)
    "procedure list any ... -> any
     like fold-multiple but works through the list elements from last to first"
    (if (null? a) r (apply proc (first a) (apply fold-multiple-right proc (tail a) r))))

  (define (fold-segments proc size init a)
    "{any:state element ... -> any:state} integer any:state list -> any
     fold over each overlapping segment with length \"size\".
     example:
     (fold-segments proc 2 #t (list 4 5 6 7))
     is equivalent to
     (proc 4 5) (proc 5 6) (proc 6 7)"
    (let loop ((rest a) (buf (list)) (r init) (count size))
      (if (null? rest) (if (null? buf) r (apply proc r buf))
        (if (< count 1)
          (loop (tail rest) (append (tail buf) (list (first rest))) (apply proc r buf) 0)
          (loop (tail rest) (append buf (list (first rest))) r (- count 1))))))

  (define (fold-unless proc stop? default init . a)
    "{any ... -> any} {any -> boolean/any} any any list ... -> any
     like fold, but returns \"default\" if \"stop?\" is true"
    (if (any null? a) init
      (apply fold-unless-check-init proc
        stop? default (apply proc (append (map first a) (list init))) (map tail a))))

  (define (fold-unless-check-init proc stop? default init . a)
    (if (stop? init) default
      (if (any null? a) init
        (apply fold-unless-check-init proc
          stop? default (apply proc (append (map first a) (list init))) (map tail a)))))

  (define (fold-until proc init stop? a)
    "procedure any procedure:{any -> boolean} list -> any
     end folding if \"stop?\" is true"
    (if (or (null? a) (stop? init)) init (fold-until proc (proc (first a) init) stop? (tail a))))

  (define (group-consecutive filter-proc a)
    "{any -> boolean} list -> list
     wrap multiple elements that consecutively match \"filter-proc\" in a list"
    (map-consecutive filter-proc (l a a) a))

  (define (improper-list-split-at-last a)
    "pair:improper-list -> (list any:non-pair)
     (1 2 . 3) -> ((1 2) 3)"
    (let loop ((rest a) (r (list)))
      (if (pair? rest) (loop (tail rest) (pair (first rest) r)) (pair (reverse r) rest))))

  (define (integer->list a)
    "any -> any/list
     wrap the argument in a list, but only if it is an integer. otherwise give the argument"
    (if (integer? a) (list a) a))

  (define (interleave a value)
    "list any -> list
     inserts value in front of each element in \"a\" except the first element.
     example: (interleave (list 1 2 3) 4) -> (1 4 2 4 3)"
    (if (null? a) a (reverse (fold (l (e r) (pairs e value r)) (list (first a)) (tail a)))))

  (define iterate-three
    (letrec
      ( (loop
          (l (proc prev current next . states)
            (if (null? next) (apply proc prev current next states)
              (apply loop proc
                (pair current prev) (first next) (tail next) (apply proc prev current next states))))))
      (l (proc a . states)
        "procedure:{list:prev any:current list:next any:state ... -> any:state ...} list any:state-init ... -> list:state
        calls \"proc\" for each list element, a list of unmodified previous list elements, a list of the following list elements
        and an arbitrary count of custom values that are updated to the result of the call to \"proc\""
        (apply loop proc (list) (first a) (tail a) states))))

  (define iterate-three-with-stop+end
    (letrec
      ( (loop
          (l (stop? end map-proc r current next . states)
            (if (or (null? next) (and stop? (apply stop? r current next states)))
              (apply (or end map-proc) r current next states)
              (apply loop stop?
                end map-proc
                (pair current r) (first next) (tail next) (apply map-proc r current next states))))))
      (l (stop? end map-proc a . states)
        "{list any list any ... -> boolean} {list any list any ... -> list:state-values}:after-stop? {list any list any ... -> list:state-values} list any ... -> any
        like \"iterate-three\" but takes two additional procedures - one for stopping the iteration after a \"map-proc\" result, and one that is called for the last element or when \"stop?\" is true"
        (loop stop? end map-proc (list) (first a) (tail a)))))

  (define (list-distribute-sorted a indices default)
    "like list-distribute but faster. works only correctly for indices lists that are sorted ascending"
    (let (last-index (last indices))
      (let loop ((index 0) (indices indices) (rest a) (r (list)))
        (if (= index last-index) (reverse (pair (first rest) r))
          (if (= index (first indices))
            (loop (+ 1 index) (tail indices) (tail rest) (pair (first rest) r))
            (loop (+ 1 index) indices rest (pair default r)))))))

  (define (list-distribute a indices default)
    "list (integer ...) any -> list
     creates a new list with values from list a at positions indices. the value for \"no-element\" is set at indices
     not included in the list indices. the length of indices must equal the length of a, and indices should not have duplicates."
    (let (indices (list-sort (l (a b) (< (first a) (first b))) (map cons indices a)))
      (list-distribute-sorted (map tail indices) (map first indices) default)))

  (define (length-one? a)
    "list -> boolean
     test if list length equals one. possibly faster than (= (length a) 1)."
    (if (null? a) #f (null? (tail a))))

  (define (length-greater-one? a)
    "list -> boolean
     true if list length is greater than one. possibly faster than (> (length a) 1).
     has-multiple-elements?"
    (if (null? a) #f (not (null? (tail a)))))

  (define* (list-index-value a value #:optional (equal-proc equal?))
    "get the index of value in list" (list-index (l (a) (equal-proc a value)) a))

  (define (list-indices proc a)
    "procedure:{any -> boolean} list -> (integer ...)
     create a list of all indices for which proc results in true"
    (let loop ((rest a) (index 0) (r (list)))
      (if (null? rest) r (loop (tail rest) (+ 1 index) (if (proc (first rest)) (pair index r) r)))))

  (define (list-prefix? a . prefix)
    "list any ... -> boolean
     true if the given \"prefix\" elements exist in order at the beginning of list.
     examples:
     (list-prefix? (list 3 2 4) 3 1) -> #f
     (list-prefix? (list 3 2 4) 3 2) -> #t"
    (let (length-prefix (length prefix))
      (if (< (length a) length-prefix) #f (equal? (take a length-prefix) prefix))))

  (define (list-suffix? a . suffix)
    "list any ... -> boolean
     true if the given \"suffix\" elements exist in order at the end of list.
     see also \"list-prefix?\""
    (let (length-suffix (length suffix))
      (if (< (length a) length-suffix) #f (equal? (take-right a length-suffix) suffix))))

  (define (split-at-last a) "list -> (list list)"
    (if (> 2 (length a)) (list a (list))
      (let (a-reverse (reverse a)) (list (reverse (tail a-reverse)) (list (first a-reverse))))))

  (define (list-replace-last a replacement)
    "list any/procedure:{any -> any} -> list
     replace the last element in a list.
     if replacement is a procedure, it is called with the last element and if the procedure result is a list then the result is appended"
    (list-replace-last-n 1 a replacement))

  (define (list-replace-last-n n a replacement)
    "list integer any/procedure:{any ... -> any/list} -> list
     if replacement is a procedure, it is called with the last \"n\" elements and if the procedure result is a list then the result is appended"
    (call-with-values (l () (split-at (reverse a) n))
      (l (replaced rest)
        (reverse
          (append
            (reverse
              (any->list (if (procedure? replacement) (apply replacement replaced) replacement)))
            rest)))))

  (define (list-select a indices)
    "list (integer ...) -> list
     return a new list consisting of values at indices"
    (map (l (e) (list-ref a e)) indices))

  (define (list-set-equal? . a)
    "list ... -> boolean
     true if all elements of the given lists appear in all others.
     uses \"equal?\" for element equality comparison"
    (apply lset= equal? a))

  (define (list-set-eqv? . a)
    "list ... -> boolean
     like \"list-set-equal?\" but uses \"eqv?\" for element equality comparison"
    (apply lset= eqv? a))

  (define (list-set-match-contains? a match-tree)
    "list list -> boolean
     test for value inclusion in list by a possibly nested conditions list like ([some/all/none] value/match-condition-tree ...).
     example:
     (list-set-match-contains? (list 1 2 3) (quote (all 2 3 (some 4 1 5) (none 8)))) -> #t"
    (list-set-match-iterate (l (e) (contains? a e)) match-tree))

  (define (list-set-match-iterate match-one? conditions)
    "procedure:{any -> boolean} list -> any:last-call-result
     evaluate a possibly nested conditions list like for \"list-set-match-contains?\".
     checking if \"match-one?\" is true for condition values in their relative condition relationships (for example some, all or none).
     \"match-one?\" is called for each individual element of the \"conditions\" list that is not a prefixed condition name,
     and its boolean results are automatically used for the conditions.
     conditions: ([some/all/none] value/conditions ...)"
    (letrec
      ( (list-set-match-iterate-internal
          (let
            (match-one?
              (l (a)
                ((if (list-set-match-condition? a) list-set-match-iterate-internal match-one?) a)))
            (l (a)
              (if (null? a) #f
                (case (first a) ((some) (any match-one? (tail a)))
                  ((all) (every match-one? (tail a))) ((none) (not (any match-one? (tail a))))
                  (else (any match-one? (list a)))))))))
      (list-set-match-iterate-internal conditions)))

  (define (list-set-match-condition? a)
    "any -> boolean
     true if \"a\" is a list-set-match condition"
    (if (list? a) (if (null? a) #f (case (first a) ((some all none) #t) (else #f))) #f))

  (define* (list-sort-by-list order a #:optional (accessor identity))
    "list list -> list
     sort a list so the elements correspond to the order of elements in list \"order\".
     elements not contained in \"order\" are moved to the end of the result list.
     examples:
     (list-sort-by-list (list 3 2 4) (list 4 2 3)) -> (3 2 4)
     (list-sort-by-list (list 3 2 4) (list 4 5 2 3)) -> (3 2 4 5)"
    (let (a-len (length a))
      (list-sort
        (l (a b)
          (< (identity-if (list-index-value order (accessor a)) a-len)
            (identity-if (list-index-value order (accessor b)) a-len)))
        a)))

  (define (list-sort-with-accessor less? accessor a)
    "procedure:{any any -> boolean} procedure:{any:list-element -> any} list -> list
     sort list by calling accessor for each argument before comparison. only the order of elements changes, the individual elements are not changed"
    (list-sort (l (a b) (less? (accessor a) (accessor b))) a))

  (define (map-selected select? proc . a)
    "procedure procedure list ... -> list
     apply proc only to elements for which \"select?\" is true. non-matched items are included in the result list.
     if multiple lists are given, it works like \"map\" except that the elements from the multiple lists for one call that are not selected are saved as a list.
     map-some/map-only"
    ;as a possible enhancement, a second procedure for non-matches could be specified
    (apply map (l e (if (apply select? e) (apply proc e) (if (null? (tail e)) (first e) e))) a))

  (define (map-apply proc . a)
    "procedure:{any ... -> any} (list ...) ... -> list
     like map but the procedure is applied with elements of \"a\" as arguments.
     example:
     (map-apply proc (list (list 1 2) (list 3 4)))
     instead of calling proc like (proc (list 1 2)) like \"map\" would do, proc is called like (proc 1 2)"
    (apply map (l e (apply proc (apply append e))) a))

  (define (map-map proc . lists)
    "procedure (list ...) ... -> list
     map with appling map to every list in list"
    (apply map (l e (apply map proc e)) lists))

  (define (map-with-continue proc . lists)
    "procedure:{procedure:{any:result}:continue any ... -> any:last-result} list ... -> list
     map over list with explicitly continuing the mapping by calling \"continue\" with the result.
     if \"continue\" is not called, the result will be the tail of the list, which means it must be a list to create a proper list.
     maps only the length of the shortest list if multiple lists are given"
    (let loop ((rest lists) (len (apply min (map length lists))))
      (if (zero? len) (list)
        (apply proc (l (result) (pair result (loop (map tail rest) (- len 1)))) (map first rest)))))

  (define (map-one match? proc a)
    "{any -> any}:predicate {any:element -> any} list -> list
     apply proc only to the first element that matches \"match?\".
     all elements that do not match are mapped with the \"identity\" function"
    (let loop ((rest a) (r (list)))
      (if (null? rest) r
        (let (e (first rest))
          (if (match? e) (append (reverse (pair (proc e) r)) (tail rest))
            (loop (tail rest) (pair e r)))))))

  (define (map-segments proc len a)
    "procedure:{any ... -> any} integer list -> list
     map over each overlapping segment of length len"
    (fold-segments (l (r . e) (append r (list (apply proc e)))) len (list) a))

  (define (map-slice slice-length proc a)
    "integer procedure:{any ... -> any} list -> list
     call \"proc\" with each \"slice-length\" number of consecutive elements of \"a\""
    (let loop ((rest a) (slice (list)) (slice-ele-length 0) (r (list)))
      (if (null? rest) (reverse (if (null? slice) r (pair (apply proc (reverse slice)) r)))
        (if (= slice-length slice-ele-length)
          (loop (tail rest) (list (first rest)) 1 (pair (apply proc (reverse slice)) r))
          (loop (tail rest) (pair (first rest) slice) (+ 1 slice-ele-length) r)))))

  (define (fold-slice slice-length proc init a)
    "integer procedure:{any:state any:element ... -> any} any list -> any:state
     call proc with each slice-length number of consecutive elements of a"
    (let loop ((rest a) (slice (list)) (slice-ele-length 0) (r init))
      (if (null? rest) (reverse (if (null? slice) r (apply proc r (reverse slice))))
        (if (= slice-length slice-ele-length)
          (loop (tail rest) (list (first rest)) 1 (apply proc r (reverse slice)))
          (loop (tail rest) (pair (first rest) slice) (+ 1 slice-ele-length) r)))))

  (define (map-consecutive filter-proc proc a)
    "{any -> boolean} {any any ... -> any} list -> list
     \"proc\" is called for and with every list of elements that consecutively matched \"filter-proc\". at least two elements at a time"
    (fold-span filter-proc
      (l (e r) (if (length-greater-one? e) (pair (apply proc e) r) (append e r))) a))

  (define (map-span filter-proc proc a)
    "procedure:{any -> any/false} procedure:{any any ... -> any} list -> list
     apply \"proc\" to each list of elements that consecutively matched \"filter-proc\". may be only one element at a time"
    (fold-span filter-proc (l (e r) (pair (apply proc e) r)) a))

  (define (map-unless proc stop? default . a)
    "procedure stop? list -> list/boolean:false
     {any -> any} {any -> boolean} list -> list/boolean
     map unless \"stop?\" is true for a mapping-result. return an empty list or \"default\" if \"stop?\" was true"
    (if (any null? a) (list)
      (let loop ((rest (map tail a)) (e (apply proc (map first a))) (init (list)))
        (if (stop? e) default
          (if (any null? rest) (reverse (pair e init))
            (loop (map tail rest) (apply proc (map first rest)) (pair e init)))))))

  (define (map-with-index proc . a)
    "procedure:{integer:index any:element ... -> any} list ... -> list"
    (let loop ((rest a) (index 0))
      (if (any null? rest) (list)
        (pair (apply proc index (map first rest)) (loop (map tail rest) (+ 1 index))))))

  (define (map-with-state proc a . init)
    "procedure list any ... -> list any ...
     {(list-element state-variable ...) -> list-element state-variable ...}
     call proc with each list element and with state variables initialised by init.
     each proc call should return a list of multiple elements, one for the mapped element, and one for each new state value."
    (apply fold-multiple (l (e r . states) (pair (apply proc e states) r)) a (list) init))

  (define (map-integers count proc)
    "integer {integer -> any} -> list
     map over integers from 0 to count - 1"
    (reverse (let loop ((n 0) (r (list))) (if (< n count) (loop (+ 1 n) (pair (proc n) r)) r))))

  (define (fold-integers count init proc)
    "integer {integer -> any} -> list
     fold over integers from 0 to count - 1"
    (let loop ((n 0) (r init)) (if (< n count) (loop (+ 1 n) (proc n r)) r)))

  (define (pair-fold-multiple proc a . init)
    "{pair any -> any} list any ... -> any
     like fold-multiple but applying proc to the pairs of list"
    (if (null? a) init (apply pair-fold-multiple proc (tail a) (apply proc a init))))

  (define (pair-map proc a)
    "procedure list -> list
     like map but not the list elements are passed to \"proc\" but the pairs of the list.
     for example (1 2 3) is just another notation for the pair notation (1 . (2 . (3 . ())))
     instead of mapping (1 2 3) pair-map maps ((1 2 3) (2 3) (3))"
    (let loop ((rest a)) (if (null? rest) (list) (pair (proc rest) (loop (tail rest))))))

  (define (pair-reverse a)
    "pair -> pair
     reverse the order of values in a pair.
     example: (pair-reverse (pair 1 2)) -> (2 . 1)"
    (pair (tail a) (first a)))

  (define (pair->list a) "pair -> list" (list (first a) (tail a)))

  (define (produce proc . a)
    "procedure:{any ... -> any} any/list ... -> list
     apply \"proc\" with each ordered combination of elements from lists, the cartesian product, and return the results in a list.
     can handle multiple lists and non-list arguments.
     for example (produce proc (1 2) (4 5) 6) is equivalent to ((proc 1 4 6) (proc 1 5 6) (proc 2 4 6) (proc 2 5 6))"
    (let loop ((rest (map any->list a)) (args (list)))
      (if (null? rest) (apply proc args)
        (let (tail-rest (tail rest))
          ( (if (null? tail-rest) map append-map) (l ele (loop tail-rest (append args ele)))
            (first rest))))))

  (define (produce-controlled proc mappers . lists)
    "{any ... -> any} (procedure:{procedure:{any -> any} list -> list} ...) any/list ... -> list
     apply \"proc\" to each ordered combination of elements from one or multiple lists, the cartesian product, and return the results in a list.
     the combinations passed to \"proc\" are obtained by nested application of the procedures in the second argument list.
     there should be as many lists as mappers.
     accepts multiple lists, multiple mappers and non-list arguments.
     example:
     (produce-controlled proc (proc-1 proc-2 proc-3) (1 2) (4 5) (6 7))
     is equivalent to
     (proc-1 (lambda (e-1) (proc-2 (lambda (e-2) (proc-3 (lambda (e-3) (proc e-1 e-2 e-3)) (6 7))) (4 5))) (1 2))"
    (let loop ((rest-mappers mappers) (rest-lists (map any->list lists)) (a (list)))
      (if (null? rest-mappers) (apply proc a)
        ( (first rest-mappers)
          (let ((tail-mappers (tail rest-mappers)) (tail-lists (tail rest-lists)))
            (l e (loop tail-mappers tail-lists (append a e))))
          (first rest-lists)))))

  (define (produce-unless proc stop? default a b)
    "{any any -> any} {any -> boolean} any list list -> any
     produce unless \"stop?\" is true for a production-result. result in false otherwise"
    (append-map-unless (l (e-1) (map-unless (l (e-2) (proc e-1 e-2)) stop? #f b)) not default a))

  (define* (replace-value a search-value replacement #:optional (equal-proc equal?))
    "list any any [procedure:{any any -> boolean}] -> list"
    (replace a (l (e) (equal-proc e search-value)) replacement))

  (define* (replace a select? replacement) "list procedure any -> list"
    (map (l (e) (if (select? e) replacement e)) a))

  (define (simplify a)
    "any/list -> list/pair/any
     list with one element -> element
     list with two non-pair elements -> pair"
    (if (list? a)
      (case (length a) ((1) (simplify (first a)))
        ( (2)
          (let ((first-ele (first a)) (second-ele (tail a)))
            (if (or (pair? first-ele) (pair? second-ele)) a (pair (first a) (first (tail a))))))
        (else a))
      a))

  (define (simplify-list a)
    "list -> list
     examples:
       (((1 2))) -> (1 2)
       (((1 2) (3))) -> ((1 2) (3))
     removes extra nesting"
    (if (null? a) a (if (and (null? (tail a)) (list? (first a))) (simplify-list (first a)) a)))

  (define (splice predicate a)
    "{list -> boolean} list -> list
     splice elements that are lists and match predicate"
    (fold-right
      (l (a result) (if (list? a) ((if (predicate a) append pair) a result) (pair a result))) (list)
      a))

  (define (splice-last-list a)
    "list -> list
     if the last element is a list, append it to the previous elements.
     example: (splice-last-list (1 2 (3 4))) -> (1 2 3 4)"
    (match a ((e ... (? list? last-e)) (append e last-e)) (_ a)))

  (define* (split-at-value a search-value #:optional inclusiveness)
    "list any [symbol:exclusive/inclusive] -> (list:left list:right)"
    (iterate-three-with-stop+end (l (prev e next . r) (equal? e search-value))
      (l (prev e next . r)
        (if (equal? e search-value)
          (if (equal? (q inclusive) inclusiveness) (list (reverse (pair e prev)) next)
            (list (reverse prev) (pair e next)))
          (list prev (pair e next))))
      (l (prev e next . r) r) a))

  (define (split-by-pattern-take-ellipsis a rest-pattern-length)
    (if (= 0 rest-pattern-length) (list a (list))
      (let (a-len (length a))
        (if (< a-len rest-pattern-length) (list #f #f)
          (call-with-values (nullary (split-at a (- a-len rest-pattern-length))) list)))))

  (define (split-by-pattern-match-ellipsis rest-pattern expr cont)
    (apply (l (match rest-expr) (cont match rest-expr rest-pattern))
      (split-by-pattern-take-ellipsis expr (length rest-pattern))))

  (define (split-by-pattern-loop pattern expr prev-name prev-value r)
    "-> (matches result)
     the first pattern has already been matched and is passed with prev-name"
    (if (null? pattern) (list (reverse (pair (pair prev-name prev-value) r)) expr)
      (if (equal? (q ...) (first pattern))
        (split-by-pattern-match-ellipsis (tail pattern) expr
          (l (match rest-expr rest-pattern)
            (if match
              (if (or (null? rest-expr) (null? pattern))
                (split-by-pattern-loop rest-pattern rest-expr prev-name (pair prev-value match) r)
                (split-by-pattern-loop (tail rest-pattern) (tail rest-expr)
                  (first rest-pattern) (first rest-expr)
                  (pair (pair prev-name (pair prev-value match)) r)))
              (list #f #f))))
        (if (null? expr)
          (if (and (= 2 (length pattern)) (equal? (q ...) (second pattern)))
            (split-by-pattern-loop (list) expr prev-name prev-value r) (list #f #f))
          (split-by-pattern-loop (tail pattern) (tail expr)
            (first pattern) (first expr) (pair (pair prev-name prev-value) r))))))

  (define (split-by-pattern pattern a)
    "(symbol symbol/ellipsis:... ...) list -> (list:matches list:rest)
     basic, fast pattern matcher that only supports variables and possibly multiple ellipses.
     binds values of list \"a\" to variables in pattern if pattern matches.
     the result is a list with two values: one for the match and one for the unmatched rest.
     if pattern did not match, then both values are false.
     unlike other pattern matchers, \"pattern\" is a list and not syntax and so can be passed as a variable"
    (if (null? pattern) (list (list) a)
      (if (null? a) (list #f #f)
        (split-by-pattern-loop (tail pattern) (tail a) (first pattern) (first a) (list)))))

  (define (pattern-match-min-length a)
    "list -> integer
     takes a flat list with symbols and ellipses and counts the required parts of a pattern with
     symbols interpreted as matching any element and ellipses to match zero or many occurences of the previous element"
    (first
      (iterate-three
        (l (p e n count)
          (list
            (if (not (or (equal? (q ...) e) (and (not (null? n)) (equal? (q ...) (first n)))))
              (+ count 1) count)))
        a 0)))

  (define* (consecutive proc a #:optional (c list))
    "procedure:{any -> any/boolean} list [procedure] -> (list:matches list:rest)
     splits the list into two lists, the first being a list of all beginning elements of \"a\" that consecutively matched
     \"proc\", the second being the rest.
     like srfi-1 span but the result is a list and not multiple return values"
    (call-with-values (nullary (span proc a)) c))

  (define (group-split-at-matches start-group? a)
    "procedure:{any -> boolean} list -> (list ...)
     wrap consecutive elements in lists with \"start-group?\" starting new lists.
     example
     (group-inline-prefixed integer? (list \"a\" \"b\" 1 \"c\" \"d\" 2 \"e\"))
     ->
     ((\"a\" \"b\") (1 \"c\" \"d\") (2 \"e\"))"
    (let (not-start-group? (negate start-group?))
      (let loop ((rest a))
        (if (null? rest) rest
          (apply (l (matches rest-2) (pair (pair (first rest) matches) (loop rest-2)))
            (consecutive not-start-group? (tail rest)))))))

  (define (insert-second e a)
    "any list -> list
     insert \"e\" as the second element into list \"a\""
    (pair (first a) (pair e (tail a))))

  (define (tail-or-null a) "list -> list" (if (null? a) a (tail a))))
