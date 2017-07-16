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

(library (sph tree)
  (export
    denoted-tree->prefix-tree
    denoted-tree->tree
    prefix-tree->denoted-tree
    prefix-tree->path-list
    prefix-tree-context-match
    prefix-tree-map
    prefix-tree-map-with-context
    prefix-tree-map-with-continue
    prefix-tree-map-with-continue-with-level
    prefix-tree-map-with-level
    prefix-tree-map-with-level->flat-list
    prefix-tree-produce
    prefix-tree-product
    prefix-tree-product-mm
    prefix-tree-replace-prefix
    prefix-tree->relations
    produce-prefix-context
    produce-prefix-context-mm
    produce-prefix-trees
    produce-prefix-trees-with-level
    produce-tree
    produce-tree-lists
    produce-tree-lists-with-levels
    produce-with-iterator-tree
    sph-tree-description
    splice-lists-without-prefix-symbol
    tree->denoted-tree
    tree-contains-any-not?
    tree-contains-any?
    tree-contains-every?
    tree-contains?
    tree-each
    tree-each-leaf
    tree-filter->flat-list
    tree-fold
    tree-fold-lists-right
    tree-fold-reverse
    tree-fold-reverse-with-level
    tree-fold-right
    tree-fold-with-level
    tree-map
    tree-map-and-self
    tree-map-leafs
    tree-map-lists
    tree-map-lists-and-self
    tree-map-lists-with-level
    tree-map-with-level
    tree-map-with-level->flat-list
    tree-map-with-state
    tree-pair->list
    tree-replace-at-once
    tree-replace-by-list
    tree-transform
    tree-transform*
    tree-transform-descend-identity)
  (import
    (sph)
    (only (guile)
      1+
      compose
      identity
      string-join)
    (only (sph alist) alist-ref)
    (only (sph list)
      contains?
      fold-integers
      flatten
      fold-multiple
      fold-segments)
    (only (srfi srfi-1)
      append-map
      fold-right
      last))

  (define sph-tree-description "processing tree-like list structures")

  (define-syntax-rule (denoted-tree->tree-inner a depth-start r-2 r update-r ...)
    (first
      (let loop ((rest a) (level depth-start) (r (list)))
        (if (null? rest) (list (reverse r))
          (let* ((e (first rest)) (depth (first e)))
            (if (< level depth)
              (apply (l (r-2 . rest) (loop rest level update-r ...)) (loop rest (+ 1 level) (list)))
              (if (> level depth) (pair (reverse r) rest)
                (loop (tail rest) level (append (tail e) r)))))))))

  (define (splice-lists-without-prefix-symbol a)
    "list -> list
     merges all lists in list an sub-lists that do not have a symbol as the first element with its parent list.
     (a 1 2 ((b 3 4) (c 5 ((d (6 7)) ())))) -> (a 1 2 (b 3 4) (c 5 (d 6 7)))
     use-case: (ice-9 peg) parser results"
    (fold-right
      (l (a r)
        (if (list? a)
          (if (null? a) (append a r)
            (let (prefix (first a))
              (if (symbol? prefix)
                (pair (pair prefix (splice-lists-without-prefix-symbol (tail a))) r)
                (append (splice-lists-without-prefix-symbol a) r))))
          (pair a r)))
      (list) a))

  (define* (denoted-tree->prefix-tree a #:optional (depth-start 0))
    "list [integer] -> list
     convert a tree representation like this ((0 a) (1 b) (2 c) (1 d)) to this (a (b c) d).
     cases like ((0 a) (3 b) (0 c)) are converted to (a ((b)) c)"
    (denoted-tree->tree-inner a depth-start
      r-2 r (if (null? r) (pair r-2 r) (pair (pair (first r) r-2) (tail r)))))

  (define* (denoted-tree->tree a #:optional (depth-start 0))
    "list:((integer any ...) ...) [integer] -> list
     convert a tree representation like this ((0 a) (1 b) (2 c) (1 d)) to this (a (b (c) d))"
    (denoted-tree->tree-inner a depth-start r-2 r (pair r-2 r)))

  (define* (produce-prefix-context proc a #:optional ignore-prefixes?)
    "{context element -> any} list -> list
     context is a list containing nested-list prefixes in reverse/upward order.
     for example (a (d e f) k (g h i) j) leads to proc applied with each of the following arguments:
     (d (a)), (e (d a)), (f (d a)),(k (a)), (g (a)), (h (g a)), (i (g a)), (j (a))
     ignore-prefix-paths ignores elements that are prefixes and only considers sub-elements"
    (if (null? a) a
      (let*
        ( (map-to-result (l (e prefix context r) (pair (proc e (pair prefix context)) r)))
          (map-to-result-list (if ignore-prefixes? (l (e prefix context r) r) map-to-result)))
        (reverse
          (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (r (list)))
            (if (null? rest) r
              (loop (tail rest) prefix
                context
                (let (e (first rest))
                  (if (and (list? e) (not (null? e)))
                    (let (prefix-next (first e))
                      (loop (tail e) prefix-next
                        (pair prefix context) (map-to-result-list prefix-next prefix context r)))
                    (map-to-result e prefix context r))))))))))

  (define (prefix-tree-context-match a pattern)
    "list list -> boolean
     true if pattern exists in prefix-tree with the same tree interpretation as prefix-tree-context-produce.
     example: (a b (d c)) contains patterns (a d c) and (a b) and (a d)"
    (if (null? a) a
      (null?
        (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (pattern pattern))
          (if (or (null? pattern) (null? rest)) pattern
            (let*
              ( (e (first rest))
                (pattern
                  (if (and (list? e) (not (null? e)))
                    (let (prefix-next (first e))
                      (loop (tail e) prefix-next
                        (pair prefix context)
                        (if (equal? prefix (first pattern)) (tail pattern) pattern)))
                    (if (equal? prefix (first pattern))
                      (let (pattern (tail pattern))
                        (if (null? pattern) pattern
                          (if (equal? e (first pattern)) (tail pattern) pattern)))
                      pattern))))
              (loop (tail rest) prefix context pattern)))))))

  (define (produce-prefix-context-mm proc a)
    "procedure:{any list -> any} list -> list
     like produce-prefix-context but allowing many-to-many relations by usings lists the first element to lists"
    (if (null? a) a
      (reverse
        (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (result (list)))
          (if (null? rest) result
            (loop (tail rest) prefix
              context
              (let (e (first rest))
                (if (list? prefix)
                  (fold
                    (if (and (list? e) (not (null? e)))
                      (let ((e-first (first e)) (e-tail (tail e)))
                        (l (prefix result)
                          (append (loop e-tail e-first (pair prefix context) (list)) result)))
                      (l (prefix result) (pair (proc e (pair prefix context)) result)))
                    result prefix)
                  (if (and (list? e) (not (null? e)))
                    (loop (tail e) (first e) (pair prefix context) result)
                    (pair (proc e (pair prefix context)) result))))))))))

  (define (tree-each proc a)
    "procedure:{any ->} list ->
     call proc for every element, lists and other elements, in tree.
     leafs left-to-right lists bottom-to-top"
    (each (l (e) (if (list? e) (begin (tree-each proc e) (proc e)) (proc e))) a))

  (define (tree-each-leaf proc a)
    "procedure:{any ->} list ->
     call proc for every non-list element in tree"
    (each (l (e) (if (list? e) (tree-each-leaf proc e) (proc e))) a))

  (define (tree-fold p r t)
    "procedure:{any:element any:result -> any:result} any:result list:tree -> any
     fold over all lists and non-list elements in tree
     non-lists from left-to-right, lists bottom-to-top"
    ;does perhaps not make much sense without an enter? predicate
    (fold (l (e r) (if (list? e) (p (tree-fold p (list) e) r) (p e r))) r t))

  (define (tree-fold-right p r t)
    "procedure:{any:element any:result -> any:result} any:result list:tree -> any
     fold over all lists and non-list elements in tree
     non-lists from left-to-right, lists bottom-to-top"
    (fold-right (l (e r) (if (list? e) (p (tree-fold-right p (list) e) r) (p e r))) r t))

  (define (tree-fold-lists-right enter? p r t)
    "procedure:{any -> boolean} procedure:{any any -> any} list -> any"
    (fold-right
      (l (e r) (if (list? e) (if (enter? e) (tree-fold-lists-right enter? p r e) (p e r)) r)) r t))

  (define (tree-fold-reverse p r t)
    "procedure:{any:element any:result -> list:result} list:result list:tree -> list
     like tree-fold but reverses result lists"
    (reverse (fold (l (e r) (if (list? e) (p (tree-fold-reverse p (list) e) r) (p e r))) r t)))

  (define* (tree-fold-with-level p r t #:optional (n 1) (inc 1+))
    "procedure:{any:element any:result integer:nesting-depth} any tree integer:level-init procedure:{integer -> integer} -> any
     like tree-fold but with additional arguments for keeping information about the nesting-depth of currently processed sub-lists"
    (fold (l (e r) (if (list? e) (p (tree-fold-with-level p (list) e (inc n) inc) r n) (p e r n)))
      r t))

  (define* (tree-fold-reverse-with-level p r t #:optional (n 1) (inc 1+))
    "procedure:{any:element any:result integer:nesting-depth} any tree integer:level-init procedure:{integer -> integer} -> any
     like tree-fold-reverse but with additional arguments for keeping information about the nesting-depth of currently processed sub-lists"
    (reverse
      (fold
        (l (e r)
          (if (list? e) (p (tree-fold-reverse-with-level p (list) e (inc n) inc) r n) (p e r n)))
        r t)))

  (define (tree-map-leafs proc a)
    "procedure:{any -> any} list -> list
     apply proc only with non-list elements, skipping but keeping list-type elements"
    (map (l (e) (if (list? e) (tree-map-leafs proc e) (proc e))) a))

  (define (tree-map-with-level->flat-list proc a . start-level)
    "procedure:{integer:nesting-depth any:element -> any:result-element} list [integer] -> (any ...)
     map elements of tree to a flat list. apply proc with a number
     stating how deeply the element is nested in other lists and the current element"
    (reverse
      (let loop ((rest a) (level (if (null? start-level) 0 (first start-level))) (r (list)))
        (if (null? rest) r
          (loop (tail rest) level
            (if (list? (first rest)) (loop (first rest) (+ 1 level) r)
              (pair (proc level (first rest)) r)))))))

  (define (produce-tree proc a b)
    "procedure:{any any -> any} list list -> any
     apply proc with every possible ordered combination of elements between two lists. iterates like tree-map"
    (tree-map (l (e-1) (tree-map (l (e-2) (proc e-1 e-2)) b)) a))

  (define (produce-tree-lists proc a b)
    "like produce-tree but pass only combinations of contained lists to proc"
    (tree-map-lists (l (e-1) (tree-map-lists (l (e-2) (proc e-1 e-2)) b)) a))

  (define (produce-with-iterator-tree iterator proc a b)
    "procedure:{proc list:elements -> any} procedure:{any:element-a any:element-b -> any}:proc list list -> any
     call proc with each ordered combination between elements of two lists with an
     iterator procedure that is called in a nested (each (lambda (e-1) (each (lambda (e-2) (proc e-1 e-2)) b)) a) way to create the argument combinations"
    (iterator
      (l (e-1)
        (if (list? e-1) (produce-with-iterator-tree iterator proc e-1 b)
          (iterator
            (l (e-2)
              (if (list? e-2) (produce-with-iterator-tree iterator proc e-1 e-2) (proc e-1 e-2)))
            b)))
      a))

  (define (prefix-tree-replace-prefix a replacements)
    "list ((to-replace . replacement) ...) -> list
     replace list prefixes (the first elements) in tree based on the given replacements structure"
    (tree-map
      (l (a level)
        (if (list? a)
          (let ((replacement (alist-ref replacements (first a))))
            (if replacement (if (procedure? replacement) (replacement a) replacement) a))
          a))
      a))

  (define (prefix-tree-produce proc a)
    "{any ... -> any} list:prefix-tree -> list
     calls proc for each combination of a prefix and tail-elements"
    (produce-prefix-context (l (context a) (apply proc (reverse (pair context a)))) a))

  (define (prefix-tree->path-list a)
    "list -> (string ...)
     regard tree as a nested list representation of a filesystem file and directory structure
     and return a flat list of corresponding filesystem path strings.
     example:
     (prefix-tree->path-list (list \"/usr\" (list \"bin\" (list \"share\" \"guile\") \"include\") \"/var\"))
     creates
     (list
       \"/usr/bin\"
       \"/usr/share/guile\"
       \"/usr/include\"
       \"/var\")"
    (prefix-tree-produce (l a (string-join a "/")) a))

  (define* (prefix-tree-product a #:optional ignore-prefixes?) "list -> list"
    "combines prefixex and tails as a one-to-many (prefix to tail elements) relation.
    example (a (d e f) (g h i) j) -> ((a d e) (a d f) (a g h) (a g i) (a j))"
    (produce-prefix-context (l (context a) (reverse (pair context a))) a ignore-prefixes?))

  (define (prefix-tree-product-mm a) "list -> list"
    "like prefix-tree-product, but also interprets lists as prefixes as many-to-many relations
    (many prefixes to tail elements).
    example ((a b) c) -> ((a c) (b c))"
    (produce-prefix-context-mm (l (context a) (reverse (pair context a))) a))

  (define (prefix-tree->relations a)
    "list -> (pair ...)
     create a list of all individual relations between prefix and tail elements or tail element prefixes.
     example:
     ((a (b c (d e)))) -> ((a . b) (b . c) (b . d) (d . e))"
    (append-map
      (l (a)
        (if (list? a)
          (let ((prefix (first a)) (suffix (tail a)))
            (append-map
              (l (a)
                (pair (pair prefix (if (list? a) (first a) a))
                  (if (list? a) (prefix-tree->relations (list a)) (list))))
              suffix))
          (list a)))
      a))

  (define (tree-map proc a)
    "procedure:{any -> any} list -> list
     maps lists bottom-to-top. does not map the topmost tree structure itself."
    (map (l (a) (proc (if (list? a) (tree-map proc a) a))) a))

  (define (tree-map-lists proc a)
    "{list -> any} list -> list
     like tree-map but pass only the lists in tree to proc, skipping and keeping non-list elements. bottom-to-top"
    (map (l (a) (if (list? a) (proc (tree-map-lists proc a)) a)) a))

  (define (tree-map-lists-and-self proc a)
    "{list -> any} list -> list
     like tree-map-lists but additionally passes the result to proc in the last call"
    (proc (tree-map-lists proc a)))

  (define (tree-map-and-self proc a)
    "{any -> any} list -> list
     like tree-map but additionally passes the result to proc in the last call"
    (proc (tree-map proc a)))

  (define* (tree-map-with-level proc a #:optional (start-level 0))
    "procedure:{any:element integer:nesting-depth} list [integer] -> list
     like tree-map but with additional arguments for the current nesting-depth"
    (let loop ((rest a) (level start-level) (r (list)))
      (if (null? rest) (proc (reverse r) level)
        (loop (tail rest) level
          (pair
            (if (list? (first rest)) (loop (first rest) (+ 1 level) (list))
              (proc (first rest) level))
            r)))))

  (define* (tree-map-lists-with-level proc a #:optional (level-init 1) (map-level 1+))
    "{list integer:current-nesting-depth -> any} list [integer {integer -> integer:next-nesting-depth-value}] -> list
     like tree-map-lists with additional arguments for the current nesting-depth"
    (let loop ((e a) (level level-init))
      (map (l (e) (if (list? e) (proc (loop e (map-level level)) level) e)) e)))

  (define (tree-map-with-state proc a . init)
    "{any any:custom-state-value ... -> list:custom-state-values} -> (list:mapped-elements any:custom-state-value ...)
     like tree-map but can carry and update a number of custom values per call, similar to fold"
    (apply (l (r . state) (apply list (reverse r) state))
      (apply fold-multiple
        (l (e r . state)
          (apply (l (map-r . state) (apply list (pair map-r r) state))
            (if (list? e) (apply tree-map-with-state proc e state) (apply proc e state))))
        a (list) init)))

  (define (prefix-tree-map proc a)
    "{any:prefix list:tail} list -> list
     map only lists, split into prefix and tail"
    (if (null? a) a
      (let (prefix (first a))
        (proc (if (list? prefix) (prefix-tree-map proc prefix) prefix)
          (map (l (e) (if (list? e) (prefix-tree-map proc e) e)) (tail a))))))

  (define* (prefix-tree-map-with-level proc a #:optional (start-depth 0))
    "{any:prefix list:tail integer:nesting-depth -> any} list [integer] -> list
     like prefix-tree-map but with an additional argument for the current nesting-depth"
    (let loop ((rest a) (depth start-depth))
      (if (null? rest) rest
        (let (prefix (first rest))
          (proc (if (list? prefix) (loop prefix (+ depth 1)) prefix)
            (map (l (e) (if (list? e) (loop e (+ depth 1)) e)) (tail rest)) depth)))))

  (define (produce-prefix-trees proc a b)
    "{any:prefix-1 any:prefix-2 any:tail-1 any:tail-2 -> any} list list -> list
     produce two prefix-trees, that is, for every odered combination of element and sub-list element of \"a\" and \"b\""
    (prefix-tree-map
      (l (prefix-1 tail-1)
        (prefix-tree-map (l (prefix-2 tail-2) (proc prefix-1 prefix-2 tail-1 tail-2)) b))
      a))

  (define (produce-prefix-trees-with-level proc a b)
    "{any:prefix-1 any:prefix-2 any:tail-1 any:tail-2 integer:depth-1 integer:depth-2 -> any} list list -> list
     like produce-prefix-trees but with an argument for the current nesting-depths"
    (prefix-tree-map-with-level
      (l (prefix-1 tail-1 level-1)
        (prefix-tree-map-with-level
          (l (prefix-2 tail-2 level-2) (proc prefix-1 prefix-2 tail-1 tail-2 level-1 level-2)) b))
      a))

  (define* (prefix-tree->infix-tree a #:optional (prefix->infix (l (p) p)))
    "list [procedure:{any -> any}] -> list
     converts list structures like (or a b (and c d)) to (a or b or (c and d))
     the optional procedure translates prefixes"
    (prefix-tree-map
      (l (prefix values)
        ( (l (prefix)
            (reverse (tail (fold (l (e prev) (pair prefix (pair e prev))) (list) values))))
          (prefix->infix prefix)))
      a))

  (define (prefix-tree-map-with-continue proc continue& a)
    "procedure:{prefix tail} procedure:{list procedure:proc procedure:continue:{list ->}} list -> list
     maps over only the lists, split into prefix and tail.
     the procedure continue& gets a procedure argument that when called continues the iteration. if it is not called,
     the iteration stops and the result of continue& is the main result"
    (let loop ((rest a))
      (if (null? rest) rest
        (proc (first rest) (map (l (e) (if (list? e) (continue& e proc loop) e)) (tail rest))))))

  (define*
    (prefix-tree-map-with-continue-with-level proc continue& a #:optional (level-proc 1+)
      (level-init 1))
    "{any:prefix list:tail any} {list procedure:proc {list ->} any}:continue list [{integer -> integer}] -> list
     like prefix-tree-map-with-continue but with additional arguments for the current nesting-depth"
    (let loop ((rest a) (level level-init))
      (if (null? rest) rest
        (proc (first rest)
          (map (l (e) (if (list? e) (continue& e proc loop (level-proc level)) e)) (tail rest)) level))))

  (define prefix-tree-map-with-context
    (letrec
      ( (loop
          (l (proc a context) "{any list:upper-prefixes} list list:start-context -> list"
            (if (null? a) a
              (proc (first a)
                (map (l (e) (if (list? e) (loop proc e (pair (first a) context)) e)) (tail a))
                context)))))
      (l (proc a)
        "{any list:upper-prefixes} list -> list
        like tree-map but with an additional arguments for upper-list prefixes"
        (loop proc a (list)))))

  (define* (produce-tree-lists-with-levels proc a b #:optional (level-proc 1+) (level-init 1))
    "{any:element-a any:element-b integer:nesting-depth-a integer:nesting-depth-b} list:list-a list:list-b [{integer -> integer} integer] -> list"
    (tree-map-lists-with-level
      (l (e-1 level-1)
        (tree-map-lists-with-level (l (e-2 level-2) (proc e-1 e-2 level-1 level-2)) b
          level-proc level-init))
      a level-proc level-init))

  (define (tree-replace-by-list a replace? replacements)
    "list {any -> boolean} (any ...) -> list
     replace each non-list element in tree that matches replace? with the next element from replacements.
     it is an error when there are replacements than matches"
    (if (null? a) a
      (if (null? replacements) replacements
        (first
          (let loop ((rest a) (r (list)) (replacements replacements))
            (if (null? rest) (list (reverse r) replacements)
              (let (a (first rest))
                (if (list? a)
                  (apply (l (a replacements) (loop (tail rest) (pair a r) replacements))
                    (loop a (list) replacements))
                  (if (replace? a)
                    (loop (tail rest) (pair (first replacements) r) (tail replacements))
                    (loop (tail rest) (pair a r) replacements))))))))))

  (define (tree-transform-ascend rest leaf-list recurse-descend ascend terminal states)
    (if (null? rest) (apply ascend (reverse leaf-list) states)
      (let (a (first rest))
        (apply
          (l (a . states)
            (tree-transform-ascend (tail rest) (pair a leaf-list)
              recurse-descend ascend terminal states))
          (apply (if (list? a) recurse-descend terminal) a states)))))

  (define (tree-transform* a descend ascend terminal . states)
    "list procedure procedure procedure any ... -> any
     descend :: any:element procedure:recurse-descend any:state ... -> (any:result-element boolean:continue? any:state ...)
     ascend :: any:element any:state ... -> (any:result-element any:state ...)
     terminal :: any:element any:state ... -> (any:result-element any:state ...)
     like tree-transform but also takes, passes and updates caller specified values"
    (letrec
      ( (recurse-descend
          (lambda (a . states)
            (if (and (list? a) (not (null? a)))
              (apply
                (l (r continue? . states)
                  (if continue?
                    (if r (apply recurse-descend r states)
                      (tree-transform-ascend a (list) recurse-descend ascend terminal states))
                    (pair r states)))
                (apply descend a recurse-descend states))
              (apply terminal a states)))))
      (apply recurse-descend a states)))

  (define (tree-transform a descend ascend terminal)
    "list procedure procedure procedure -> any
     descend :: any:element procedure:recurse-descend -> (any:result-element boolean:continue?)
     ascend :: any:element -> any:result-element
     terminal :: any:element -> any:result-element
     map/transform list and sub-lists first top to bottom, calling \"descend\" for each sub-list,
     then bottom to top, calling \"ascend\" for lists and \"terminal\" for non-lists/leafs.
     descend should return a list of two values: the first for the result and the second a boolean indicating if the result
     should be passed to ascend and terminal (true) or if that should be skipped (false).
     example use cases:
     * compile s-expression list trees into string output languages by mapping all expressions and sub-expressions to strings
     * apply transformations to a list tree"
    (first
      (tree-transform* a (l (a re-descend) (descend a (compose first re-descend)))
        (compose list ascend) (compose list terminal))))

  (define (tree-transform-descend-identity . a)
    "any ... -> (#f #t)
     a tree-transform descend procedure that does not do anything"
    (list #f #t))

  (define (tree-replace-at-once match? proc a)
    "procedure:{element -> boolean} procedure:{list:matched-elements -> list} list -> list
     searches through tree recursively, collecting all elements (including lists) that match collect-proc, then calls
     proc with a list of matched elements. the result of proc must of length zero (no replacement) or matched-element-count (replaces all matches).
     results in the tree with the matched elements are replaced in order by the result elements from calling proc"
    (tree-replace-by-list a match? (proc (tree-filter->flat-list match? a))))

  (define* (tree-contains? a search-value #:optional (equal-proc equal?))
    "list any [procedure:{any any -> boolean}] -> boolean
     compares all (non-list and list) elements to search-value"
    (any (l (e) (or (equal-proc e search-value) (and (list? e) (tree-contains? e search-value)))) a))

  (define (tree-contains-any? a equal-proc . search-values)
    "list {any any -> boolean} any ... -> boolean
     like tree-contains? and true if any of the search-values is found"
    (any (l (search-value) (tree-contains? a search-value equal-proc)) search-values))

  (define (tree-contains-every? a equal-proc . search-values)
    "list {any any -> boolean} any ... -> boolean
     like tree-contains? but true only if every of the search-values has been found"
    (every (l (search-value) (tree-contains? a search-value equal-proc)) search-values))

  (define* (tree-contains-any-not? a equal-proc . search-values)
    "list {any any -> boolean} any ... -> boolean
     like tree-contains? but true only if any of the search-values is not contained"
    (any
      (l (e)
        (if (list? e) (apply tree-contains-any-not? e search-values)
          (not (contains? search-values e equal-proc))))
      a))

  (define (tree->denoted-tree a . start-level)
    "list [integer] -> list
     convert a tree to an association list where each element is a list having a nesting-depth number
     as the first element. similar to this is the usage of indentation for nesting depth.
     (a b (c (d e)) f) -> ((0 . a) (0 . b) (1 . c) (2 . d) (2 . e) (0 . f))"
    (apply tree-map-with-level->flat-list list a start-level))

  (define (prefix-tree-map-with-level->flat-list proc a . start-level)
    "{integer any -> any} list [integer] -> (any ...)
     like tree->denoted-tree->flat-list but the nesting-depth number corresponds to prefix-tree interpretation"
    (reverse
      (let loop
        ((rest a) (level (if (null? start-level) 0 (first start-level))) (is-prefix #t) (r (list)))
        (if (null? rest) r
          (loop (tail rest) level
            #f
            (if (list? (first rest)) (loop (first rest) (+ 1 level) #t r)
              (pair
                (proc (if (and is-prefix (> level 0) (not (null? (tail rest)))) (- level 1) level)
                  (first rest))
                r)))))))

  (define (prefix-tree->denoted-tree a . start-level)
    "list [integer] -> list
     like tree->denoted-tree but the nesting-depth number corresponds to prefix-tree interpretation. example
     (a b (c (d e)) f) -> ((0 . a) (1 . b) (2 . c) (3 . d) (3 . e) (0 . 4))"
    (apply prefix-tree-map-with-level->flat-list list a start-level))

  (define (tree-filter->flat-list proc a)
    "procedure:{any -> boolean} list -> list
     results in a flat list of all elements (non-lists and lists) of tree that matched for which proc resulted in a true value"
    (let loop ((rest a) (r (list)))
      (if (null? rest) r
        (loop (tail rest)
          (let (e (first rest)) (if (proc e) (pair e r) (if (list? e) (loop e r) r)))))))

  (define (tree-pair->list a)
    "list -> list
     converts all non-list pairs in tree to lists"
    (tree-map (l (e) (if (pair? e) (list (first e) (tail e)) e)) a)))
