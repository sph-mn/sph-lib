; (sph list) - list processing
; written for the guile scheme interpreter
; Copyright (C) 2010-2015 sph <sph@posteo.eu>
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
    collect-map
    complement
    complement-both
    contains-all?
    contains-some?
    contains?
    containsq?
    containsv?
    count-occurence
    count-occurence-with-limit
    count-with-limit
    define-list
    delete-duplicates-sorted
    difference
    difference+intersection
    difference+intersection-p
    difference-p
    each-first-middle-last
    each-in-index-range
    each-slice
    each-with-index
    every-map
    filter-append-map
    filter-not
    filter-produce
    first-if-single
    first-intersection
    first-intersection-p
    first-or-false
    first-or-null
    flat?
    fold-multiple
    fold-multiple-right
    fold-segments
    fold-span
    fold-unless
    fold-unless-check-init
    fold-until
    fold-with-buffer
    group-successive
    improper-list-split-at-last
    insert-second
    integer->list
    interleave
    intersection
    intersection-p
    iterate-three
    iterate-three-with-stop+end
    iterate-with-continue
    length-eq-one?
    length-greater-one?
    list-bind
    list-distribute
    list-distribute-sorted
    list-index-value
    list-indices
    list-prefix?
    list-replace-last
    list-replace-last-n
    list-select
    list-set-equal?
    list-set-eqv?
    list-set-match-condition?
    list-set-match-contains?
    list-sort-by-list
    list-sort-by-list-with-accessor
    list-sort-with-accessor
    map-apply
    map-map
    map-only
    map-segments
    map-slice
    map-span
    map-successive
    map-unless
    map-with-index
    map-with-state
    n-times-fold
    n-times-map
    pair->list
    pair-fold-multiple
    pair-map
    pair-reverse
    pattern-match-min-length
    produce
    produce-controlled
    produce-unless
    replace
    simplify
    simplify-list
    splice
    splice-last-list
    split-at-value
    split-by-pattern
    successive
    true->list
    true->list-s
    (rename (lset-adjoin list-set-add)
      (list-tail tail-ref)
      (lset-difference complement-p)
      (lset<= list-set-subset?)))
  (import
    (ice-9 match)
    (sph)
    (srfi srfi-31)
    (except (rnrs base) map)
    (only (guile)
      filter
      inf
      identity
      map
      memv
      memq
      member)
    (only (rnrs sorting) list-sort)
    (only (srfi srfi-1)
      append-map
      last
      find
      alist-cons
      delete-duplicates
      filter-map
      fold-right
      list-index
      lset-difference
      lset=
      lset-adjoin
      lset<=
      span
      split-at
      take))

  (define-syntax-rule (identity-if test else ...) ((lambda (r) (if r r (begin else ...))) test))

  (define-syntax-rule (list-bind a lambda-formals body ...)
    (apply (lambda lambda-formals body ...) a))

  (define (append-map-unless proc stop? default . a)
    "procedure stop? list -> list/false
    {list-element -> list-element} {procedure-result -> boolean}
    map unless stop? is true for a mapping-result. return a new list or false if a false result occured."
    (if (any null? a) (list)
      (let loop ((rest (map tail a)) (cur (apply proc (map first a))) (init (list)))
        (if (stop? cur) default
          (if (any null? rest) (append init cur)
            (loop (map tail rest) (apply proc (map first rest)) (append init cur)))))))

  (define (fold-every proc init . a)
    "{any any -> any} any list ...
    like fold, but every result must be a \"true\" value, otherwise the result
    is the false result."
    (if init (if (null? a) init (fold-every proc (proc (map first a) init) (map tail a))) init))

  (define (any->list a) "wraps a non-list argument in a list" (any->list-s a))
  (define-syntax-rule (any->list-s a) (if (list? a) a (list a)))
  (define-syntax-rule (true->list-s a) (if a (any->list-s a) a))
  (define (true->list a) "wraps a true non-list argument in a list" (true->list-s a))

  (define (collect-map collect-proc proc a)
    "procedure:{any -> boolean} procedure:{list:matched-elements -> list:replacements} list:source -> list
    applies proc with a list of all elements matching collect-proc.
    the result must be a list of zero to matched-elements-count length and is used to
    replace matched elements in order in the source list. collect-proc is evaluated only once per source element"
    (let (collected (map (l (e) (pair (collect-proc e) e)) a))
      (fold-multiple
        (l (e r replacements)
          (if (first e) (pair (pair (first replacements) r) (tail replacements))
            (pair (pair (tail e) r) replacements)))
        a (list) (proc (map tail collected)))))

  (define (contains-all? a . values)
    "list ... -> boolean
    test if argument one contains all elements from following lists"
    (every (l (a-2) (every (l (e) (contains? a e)) a-2)) values))

  (define (contains-some? a . values)
    "list ... -> boolean
    test if argument one contains at least one element from following lists"
    (any (l (a-2) (any (l (e) (contains? a e)) a-2)) values))

  (define* (contains? a value #:optional (member member))
    "list any [member-proc] -> boolean
    return a boolean indicating if list a contains object value"
    (if (member value a) #t #f))

  (define* (containsq? a value) (if (memq value a) #t #f))
  (define* (containsv? a value) (if (memv value a) #t #f))

  (define (count-occurence value a)
    "any list -> integer
    count occurences of a value in list"
    (let loop ((rest (member value a)) (count 0))
      (if (pair? rest) (loop (member value (tail rest)) (+ count 1)) count)))

  (define* (count-occurence-with-limit value a #:optional (count-limit (inf)) (member member))
    "any list integer procedure -> integer
    like count-occurence but allows to specify a count at which to stop counting"
    (let loop ((rest (member value a)) (count 0))
      (if (pair? rest)
        (let (count (+ count 1))
          (if (= count-limit count) count (loop (member value (tail rest)) count)))
        count)))

  (define* (count-with-limit pred limit . a)
    "procedure integer list ... -> integer
    like count but allows to specify a count limit at which to stop counting"
    (let loop ((rest a) (count 0))
      (if (any null? rest) count
        (if (apply pred (map first rest))
          (let (count (+ 1 count)) (if (= count limit) count (loop (map tail rest) count)))
          (loop (map tail rest))))))

  (define-syntax-rule (define-list name args ...) (define name (list args ...)))

  (define (complement-both a b)
    "list list -> (list list)
    delete elements in both lists that are included in both lists"
    (list (complement a b) (complement b a)))

  (define (complement . lists)
    "list ... -> list
    delete elements from the first list that are included in the other lists"
    (apply lset-difference equal? lists))

  (define* (delete-duplicates-sorted a #:optional (equal-proc equal?) (preserve-order #t))
    "delete duplicates from a sorted list. faster algorithm than for unsorted lists."
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

  (define (each-in-index-range proc start end . a) "untested"
    (let loop ((rest a) (index (if (< end 0) (+ end (length (first a))) 0)))
      (if (<= index end)
        (begin (if (>= index start) (apply proc (map first rest))) (loop (map tail rest))))))

  (define (each-first-middle-last first-proc middle-proc last-proc . a) "untested"
    (apply first-proc (map first a))
    (let loop ((rest (map tail a)) (count (- (length (first a)) 1)))
      (if (= 0 count) (apply last-proc (map first rest))
        (begin (middle-proc (map first rest)) (loop (map tail rest) (- count 1))))))

  (define (every-map proc . a)
    "procedure:{any -> any} list ... -> list/false
    like map but results in false if any result of proc is not a true value"
    (apply map-unless proc not #f a))

  (define (filter-append-map proc . lists) "apply filter-map and then apply append on the result"
    (apply append (apply filter-map proc lists)))

  (define (difference-p equal-proc . lists)
    "{any any -> boolean} list ... -> list
    like "
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
    like "
    difference+intersection " but the predicate for comparing list elements can be specified"
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
    like "
    intersection " but the predicate for comparing list elements can be specified"
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
    "apply proc to each ordered combination of elements (cartesian product) from lists and return the results in a list.
    can handle multiple lists and atomic arguments.
    (produce proc (1 2) (4 5) (6)) == (list (proc 1 4 6) (proc 1 5 6) (proc 2 4 6) (proc 2 5 6))"
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
    result in the first found element that is included in both lists"
    (first-intersection-p equal? a b))

  (define (first-if-single a) (if (or (null? a) (not (null? (tail a)))) a (first a)))

  (define (first-or-false a)
    "list -> any/false
    results in the first element of a list if it is not null, otherwise false"
    (if (null? a) #f (first a)))

  (define (first-or-null a)
    "results in the first element of a list if it is not null, otherwise null"
    (if (null? a) a (first a)))

  (define (flat? a)
    "list -> boolean
    true if the list has no list as an element"
    (if (null? a) #t (if (list? (first a)) #f (flat? (tail a)))))

  (define (fold-span filter-proc proc a)
    "procedure:{any -> any/false} procedure:{list -> any} list -> any
    fold over each list of elements that successively matched filter-proc (utilising the "
    span " procedure)"
    (let loop ((rest a) (r (list)))
      (if (null? rest) (reverse r)
        (let-values (((successive rest) (span filter-proc rest)))
          (if (null? successive) (loop (tail rest) (pair (first rest) r))
            (loop rest (proc successive r)))))))

  (define (fold-multiple proc a . r)
    "{any ... -> list} list any ... -> list
    {previous-result-values ... -> list}
    like fold but with multiple state values.
    apply proc to the elements of \"a\" with list elements that were the result of the previous call or given init arguments if it is the first call"
    (if (null? a) r (apply fold-multiple proc (tail a) (apply proc (first a) r))))

  (define (fold-multiple-right proc a . r) ""
    (if (null? a) r (apply proc (first a) (apply fold-multiple-right proc (tail a) r))))

  (define (fold-segments proc len init a)
    "integer {any:state element ... -> any:state} any:state list -> any
    fold over each overlapping segment of length \"len\""
    (let loop ((rest a) (buf (list)) (r init) (count len))
      (if (null? rest) (if (null? buf) r (apply proc r buf))
        (if (< count 1)
          (loop (tail rest) (append (tail buf) (list (first rest))) (apply proc r buf) 0)
          (loop (tail rest) (append buf (list (first rest))) r (- count 1))))))

  (define (fold-unless proc stop? default init . a)
    "{any any -> any} {any -> boolean} any any list ... -> any
    like fold, but returns \"default\" if stop? evaluates to true"
    (if (any null? a) init
      (apply fold-unless-check-init proc
        stop? default (apply proc (append (map first a) (list init))) (map tail a))))

  (define (fold-unless-check-init proc stop? default init . a)
    (if (stop? init) default
      (if (any null? a) init
        (apply fold-unless-check-init proc
          stop? default (apply proc (append (map first a) (list init))) (map tail a)))))

  (define (fold-until proc init stop? a)
    "procedure any procedure:{any -> boolean} list
    end folding if stop? results in true"
    (if (or (stop? init) (null? a)) init (fold-until proc (proc (first a) init) stop? (tail a))))

  (define (group-successive filter-proc a)
    "{any -> boolean} list -> list
    wrap multiple, sucessive elements matching filter-proc in a list"
    (map-successive filter-proc (l args args) a))

  (define (improper-list-split-at-last a)
    "improper-list -> (list . non-pair)
    (1 2 . 3) -> (1 2) 3"
    (let loop ((rest a) (r (list)))
      (if (pair? rest) (loop (tail rest) (pair (first rest) r)) (pair (reverse r) rest))))

  (define (integer->list a)
    "any -> any/list
    if a is an integer, wrap it in a list, else return it"
    (if (integer? a) (list a) a))

  (define (interleave a value)
    "list any -> list
    inserts value in a in front of each element except for the first"
    (if (null? a) a (reverse (fold (l (e r) (pairs e value r)) (list (first a)) (tail a)))))

  (define iterate-three
    (letrec
      ( (loop
          (l (proc prev current next . states)
            (if (null? next) (apply proc prev current next states)
              (apply loop proc
                (pair current prev) (first next) (tail next) (apply proc prev current next states))))))
      (l (proc a . states)
        "procedure:{prev:list current:any next:list state:any ... -> state:any ...} list state-init:any ...
        calls proc for each list element, a list of unmodified previous list elements, a list of the next list elements
        and an arbitrary count of custom values that are updated from the list-result of each call to proc"
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
        like iterate-three but takes two additional procedures - one for stopping the iteration with the current result, and one that is called for the last element or when stop? is true"
        (loop stop? end map-proc (list) (first a) (tail a)))))

  (define (iterate-with-continue proc a . states)
    "procedure:{any:element list:rest procedure:continue:{list:next-pair any:state-value ...} any:state-value ... -> any} list any:state-value ..."
    (apply
      (rec (loop rest . states)
        (if (null? rest) states (apply proc (first rest) (tail rest) loop states)))
      a states))

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

  (define (length-eq-one? a)
    "list -> boolean
    test if list length equals one. possibly faster than (= (length a) 1)."
    (if (null? a) #f (null? (tail a))))

  (define (length-greater-one? a)
    "list -> boolean
    test if list length is greater than one. possibly faster than (> (length a) 1)."
    (if (null? a) #f (not (null? (tail a)))))

  (define* (list-index-value a value #:optional (equal-proc equal?))
    "get the index of value in list" (list-index (l (e) (equal-proc e value)) a))

  (define (list-indices proc a)
    "procedure:{any -> boolean} list -> (integer ...)
    create a list of all indices for which proc results in true"
    (let loop ((rest a) (index 0) (r (list)))
      (if (null? rest) r (loop (tail rest) (+ 1 index) (if (proc (first rest)) (pair index r) r)))))

  (define (list-prefix? a . prefix)
    "list any ... -> boolean
    check if the list of prefixes is a prefix of list"
    (let (length-prefix (length prefix))
      (if (< (length a) length-prefix) #f (equal? (take a length-prefix) prefix))))

  (define (list-replace-last a replacement)
    "list any/{any ... -> any} -> list
    if replacement is a procedure and the result is a list, the arguments are spliced into the result."
    (list-replace-last-n 1 a replacement))

  (define (list-replace-last-n n a replacement)
    "list integer any/procedure -> list
    if replacement is a procedure and the result is a list, the arguments are spliced into the result."
    (call-with-values (l () (split-at (reverse a) n))
      (l (replaced rest)
        (reverse
          (append
            (reverse
              (any->list (if (procedure? replacement) (apply replacement replaced) replacement)))
            rest)))))

  (define (list-select a indices)
    "list (integer ...) -> list
    return a new list consisting of values at indices of list lis"
    (map (l (e) (list-ref a e)) indices))

  (define (list-set-equal? . args) "list ... -> boolean" (apply lset= equal? args))
  (define (list-set-eqv? . args) "list ... -> boolean" (apply lset= eqv? args))

  (define (list-set-match-contains? a match-tree)
    "test for value inclusion in list by a match-condition-tree like ([some/all/none] value/match-condition-tree ...)"
    (list-set-match-iterate (l (e) (contains? a e)) match-tree))

  (define (list-set-match-iterate proc a)
    "procedure list -> last-call-result
    iterate over elements which are not match-conditions"
    (letrec
      ( (list-set-match-iterate-internal
          (let
            (proc
              (l (a) ((if (list-set-match-condition? a) list-set-match-iterate-internal proc) a)))
            (l (a)
              (if (null? a) #f
                (case (first a) ((some) (any proc (tail a)))
                  ((all) (every proc (tail a))) ((none) (not (any proc (tail a))))
                  (else (any proc (list a)))))))))
      (list-set-match-iterate-internal a)))

  (define (list-set-match-condition? a)
    "([some all none] e ...)
    results in true if list is a match-condition"
    (if (list? a) (if (null? a) #f (case (first a) ((some all none) #t) (else #f))) #f))

  (define (list-sort-by-list order a)
    "sort a list corresponding to order of elements from list order"
    (let (a-len (length a))
      (list-sort
        (l (a b)
          (< (identity-if (list-index-value order a) a-len)
            (identity-if (list-index-value order b) a-len)))
        a)))

  (define (list-sort-by-list-with-accessor order accessor a)
    "sort a list corresponding to order of elements from list order"
    (let (a-len (length a))
      (list-sort
        (l (a b)
          (< (identity-if (list-index-value order (accessor a)) a-len)
            (identity-if (list-index-value order (accessor b)) a-len)))
        a)))

  (define (list-sort-with-accessor less? accessor a)
    "procedure:{any any -> boolean} procedure:{any:list-element -> any} list -> list
    sort list by applying accessor for each argument before comparison. only the order changes, the elements stay the same"
    (list-sort (l (a b) (less? (accessor a) (accessor b))) a))

  (define (map-apply proc . a)
    "procedure:{any ...} list:(list ...)
    like map but the procedure is applied with the elements of elements as arguments"
    (apply map (l e (apply proc (apply append e))) a))

  (define (map-map proc . lists)
    "((any ...) ...) -> ((any ...) ...)
    apply map for every list in list"
    (apply map (l e (apply map proc e)) lists))

  (define (map-only filter-proc proc a)
    "procedure:{any -> any/false} procedure:{any ... -> list} list -> list
    apply proc only for elements that successively matched filter-proc.
    proc is supposed to result in a list of results so multiple elements may be replaced at once."
    (fold-span filter-proc (l (e r) (pair (apply proc e) r)) a))

  (define (map-segments proc len a)
    "
    procedure:{any ... -> any} integer list -> list
    map over each overlapping segment of length len"
    (fold-segments (l (r . e) (append r (list (apply proc e)))) len (list) a))

  (define (map-slice slice-length proc a)
    "{any ... -> any} integer list
    call proc with each slice-length number of successive elements of a"
    (let loop ((rest a) (slice (list)) (slice-ele-length 0) (r (list)))
      (if (null? rest) (reverse (if (null? slice) r (pair (apply proc (reverse slice)) r)))
        (if (= slice-length slice-ele-length)
          (loop (tail rest) (list (first rest)) 1 (pair (apply proc (reverse slice)) r))
          (loop (tail rest) (pair (first rest) slice) (+ 1 slice-ele-length) r)))))

  (define (map-successive filter-proc proc a)
    "{any -> boolean} {any any ... -> any} list -> list
    call proc with with each list greater than one of elements that successively matched filter-proc."
    (fold-span filter-proc
      (l (e r) (if (length-greater-one? e) (pair (apply proc e) r) (append e r))) a))

  (define (map-span filter-proc proc a)
    "procedure:{any -> any/false} procedure:{any any ... -> any} list -> list
    apply proc with with each list of elements that successively matched filter-proc."
    (fold-span filter-proc (l (e r) (pair (apply proc e) r)) a))

  (define (map-unless proc stop? default . a)
    "procedure stop? list -> list/false
    {any -> any} {any -> boolean}
    map unless stop? is true for a mapping-result. return a new list or default if a stop? was true."
    (if (any null? a) (list)
      (let loop ((rest (map tail a)) (cur (apply proc (map first a))) (init (list)))
        (if (stop? cur) default
          (if (any null? rest) (reverse (pair cur init))
            (loop (map tail rest) (apply proc (map first rest)) (pair cur init)))))))

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

  (define (n-times-map count proc)
    "integer {integer -> any} -> list
    map over the numbers 0 to count"
    (reverse (let loop ((n 0) (r (list))) (if (< n count) (loop (+ 1 n) (pair (proc n) r)) r))))

  (define (n-times-fold count init proc)
    "integer {integer -> any} -> list
    fold over the numbers 0 to count"
    (let loop ((n 0) (r init)) (if (< n count) (loop (+ 1 n) (proc n r)) r)))

  (define (pair-fold-multiple proc a . init)
    "{pair any -> any} list any ... -> any
    like fold-multiple but applying proc to the pairs of list"
    (if (null? a) init (apply pair-fold-multiple proc (tail a) (apply proc a init))))

  (define (pair-map proc a)
    "like map but not the list elements are passed to \"proc\" but the pairs of the list.
    lists are made of pairs, for example (1 2 3) is just another notation for (1 . (2 . (3 . ())))"
    (let loop ((rest a)) (if (null? rest) (list) (pair (proc rest) (loop (tail rest))))))

  (define (pair-reverse a)
    "
    pair -> pair
    reverses the order of the two values in a pair"
    (pair (tail a) (first a)))

  (define (pair->list a) (list (first a) (tail a)))

  (define (produce proc . a)
    "procedure:{any ... -> any} any/list ... -> list
    apply proc to each ordered combination of elements (cartesian product) from lists and return the results in a list.
    can handle multiple lists and non-list arguments.
    (produce proc (1 2) (4 5) (6)) == (list (proc 1 4 6) (proc 1 5 6) (proc 2 4 6) (proc 2 5 6))"
    (let loop ((rest (map any->list a)) (args (list)))
      (if (null? rest) (apply proc args)
        (let (tail-rest (tail rest))
          ( (if (null? tail-rest) map append-map) (l ele (loop tail-rest (append args ele)))
            (first rest))))))

  (define (produce-controlled proc procs . lists)
    "{any ... -> any} ({procedure:{any -> any} list} ...) any/list ... -> list
    apply proc to each ordered combination of elements (cartesian product) from one or multiple lists and return the results in a list.
    the combinations for proc are obtained by nested application of the procedures in the second argument.
    there should be as many lists as iteration-procedures.
    accepts multiple lists, multiple iteration-procedures and non-list arguments.
    (produce-controlled proc (proc-1 proc-2 proc-3) (1 2) (4 5) (6 7))
    =>
    (proc-1 (lambda (ele-1)
    (proc-2 (lambda (ele-2)
    (proc-3 (lambda (ele-3)
    (proc ele-1 ele-2 ele-3))
    (6 7)))
    (4 5)))
    (1 2))"
    (let loop ((rest-procs procs) (rest-lists (map any->list lists)) (args (list)))
      (if (null? rest-procs) (apply proc args)
        ( (first rest-procs)
          (let ((tail-procs (tail rest-procs)) (tail-lists (tail rest-lists)))
            (l e (loop tail-procs tail-lists (append args e))))
          (first rest-lists)))))

  (define (produce-unless proc stop? default a b)
    "{any any -> any} {any -> boolean} any list list -> any
    produce unless stop? is true for a production-result. result in false otherwise."
    (append-map-unless (l (e-1) (map-unless (l (e-2) (proc e-1 e-2)) stop? #f b)) not default a))

  (define* (replace a search-value replacement #:optional (equal-proc equal?))
    "list any any -> list" (map (l (e) (if (equal-proc e search-value) replacement e)) a))

  (define (simplify a)
    "list with one element -> element
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
    example (((1 2))) -> (1 2)"
    (if (null? a) a (if (and (null? (tail a)) (list? (first a))) (simplify-list (first a)) a)))

  (define (splice proc a)
    "{list -> boolean} -> list
    for each sub-list, merge sub-list at-place into parent list if proc result is true."
    (fold-right (l (e r) (if (list? e) ((if (proc e) append cons) e r) (pair e r))) (list) a))

  (define (splice-last-list a)
    "list -> list
    if the last element is a list, append it to the previous elements."
    (match a ((e ... (? list? last-e)) (append e last-e)) (_ a)))

  (define* (split-at-value a search-value #:optional inclusiveness)
    "list any [symbol:exclusive/inclusive] -> (list:left list:right)"
    (iterate-three-with-stop+end (l (prev e next . r) (equal? e search-value))
      (l (prev e next . r)
        (if (equal? e search-value)
          (if (eq? (q inclusive) inclusiveness) (list (reverse (pair e prev)) next)
            (list (reverse prev) (pair e next)))
          (list prev (pair e next))))
      (l (prev e next . r) r) a))

  (define (split-by-pattern-take-ellipsis a len)
    (if (= 0 len) (list a (list))
      (let (a-len (length a))
        (if (<= a-len len) (list #f #f) (call-with-values (l () (split-at a (- a-len len))) list)))))

  (define (split-by-pattern-match-ellipsis rest-pattern expr cont)
    (apply (l (match rest-expr) (cont match rest-expr rest-pattern))
      (split-by-pattern-take-ellipsis expr (length rest-pattern))))

  (define (split-by-pattern-loop pattern expr prev-name prev-value r) "-> (matches result)"
    (if (null? pattern) (list (reverse (pair (pair prev-name prev-value) r)) expr)
      (if (eqv? (q ...) (first pattern))
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
          (if (and (= 2 (length pattern)) (eq? (q ...) (second pattern)))
            (split-by-pattern-loop (list) expr prev-name prev-value r) (list #f #f))
          (split-by-pattern-loop (tail pattern) (tail expr)
            (first pattern) (first expr) (pair (pair prev-name prev-value) r))))))

  (define (split-by-pattern pattern a)
    "(symbol symbol/... ...) list -> (matches rest)
    basic/fast pattern matcher that supports only variables and ellipses. the result is a list with two values, one for the match and one for the unmatched rest.
    if pattern did not match then both values are false.
    unlike with other pattern matchers the pattern is a list and not syntax"
    (if (null? pattern) (list (list) a)
      (if (null? a) (list #f #f)
        (split-by-pattern-loop (tail pattern) (tail a) (first pattern) (first a) (list)))))

  (define (pattern-match-min-length a)
    "takes a flat list with symbols and ellipses and counts the required parts of a pattern with
    symbols interpreted as matching anything and ellipses to match zero or many occurences of the previous element"
    (first
      (iterate-three
        (l (p e n count)
          (list
            (if (not (or (eqv? (q ...) e) (and (not (null? n)) (eqv? (q ...) (first n)))))
              (+ count 1) count)))
        a 0)))

  (define (successive proc a)
    "procedure:{any -> any/boolean} list -> (matches rest)
    splits the list into two lists, the first being a list of all beginning elements of a that successively match
    proc, the second being the rest.
    like srfi-1 span but results in a list instead of multiple values"
    (call-with-values (l () (span proc a)) (l r r)))

  (define (insert-second e a)
    "any list -> list
    insert as the second element into a list"
    (pair (first a) (pair e (tail a)))))