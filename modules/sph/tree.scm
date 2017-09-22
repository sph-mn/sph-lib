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
    denoted-tree-adjust-depth
    denoted-tree-minimise-depth
    prefix-tree->denoted-tree
    prefix-tree->paths
    prefix-tree->relations
    prefix-tree-context-match
    prefix-tree-map
    prefix-tree-map-with-context
    prefix-tree-map-with-continue
    prefix-tree-map-with-continue-with-depth
    prefix-tree-map-with-depth
    prefix-tree-map-with-depth->flat-list
    prefix-tree-produce
    prefix-tree-produce-with-context
    prefix-tree-produce-with-context-mm
    prefix-tree-product
    prefix-tree-product-mm
    tree-fold-right-depth
    prefix-tree-replace-prefix
    produce-prefix-trees
    produce-prefix-trees-with-depth
    produce-with-iterator-tree
    sph-tree-description
    splice-lists-without-prefix-symbol
    tree->denoted-tree
    tree-contains-all?
    tree-contains-some-not?
    tree-contains-some?
    tree-contains?
    tree-each
    tree-each-leaf
    tree-filter->flat-list
    tree-fold
    tree-fold-lists-right
    tree-fold-reverse
    tree-fold-reverse-with-depth
    tree-fold-right
    tree-fold-with-depth
    tree-map
    tree-map-and-self
    tree-map-leafs
    tree-map-lists
    tree-map-lists-and-self
    tree-map-lists-with-depth
    tree-map-with-depth
    tree-map-with-depth->flat-list
    tree-map-with-state
    tree-pair->list
    tree-produce
    tree-produce-lists
    tree-produce-lists-with-depth
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
      (let loop ((rest a) (depth depth-start) (r (list)))
        (if (null? rest) (list (reverse r))
          (let* ((b (first rest)) (depth-2 (first b)))
            (if (< depth depth-2)
              (apply (l (r-2 . rest) (loop rest depth update-r ...)) (loop rest (+ 1 depth) (list)))
              (if (> depth depth-2) (pair (reverse r) rest)
                (loop (tail rest) depth (pair (tail b) r)))))))))

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

  (define (tree-each proc a)
    "procedure:{any ->} list ->
     call proc for every element, lists and other elements, in tree.
     leafs left-to-right lists bottom-to-top"
    (each (l (e) (if (list? e) (begin (tree-each proc e) (proc e)) (proc e))) a))

  (define (tree-each-leaf proc a)
    "procedure:{any ->} list ->
     call proc for every non-list element in tree"
    (each (l (b) (if (list? b) (tree-each-leaf proc b) (proc b))) a))

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
    "procedure:{any -> boolean} procedure:{any any -> any} list -> any
    experimental."
    (fold-right
      (l (e r) (if (list? e) (if (enter? e) (tree-fold-lists-right enter? p r e) (p e r)) r)) r t))

  (define (tree-fold-reverse p r t)
    "procedure:{any:element any:result -> list:result} list:result list:tree -> list
     like tree-fold but reverses result lists"
    (reverse (fold (l (e r) (if (list? e) (p (tree-fold-reverse p (list) e) r) (p e r))) r t)))

  (define* (tree-fold-with-depth p r t #:optional (n 1) (inc 1+))
    "procedure:{any:element any:result integer:nesting-depth} any tree integer:depth-init procedure:{integer -> integer} -> any
     like tree-fold but with additional arguments for keeping information about the nesting-depth of currently processed sub-lists"
    (fold (l (e r) (if (list? e) (p (tree-fold-with-depth p (list) e (inc n) inc) r n) (p e r n)))
      r t))

   (define* (tree-fold-right-depth p r t #:optional (depth 1) (inc 1+))
    "procedure:{any:element any:result -> any:result} any:result list:tree -> any
     fold over all lists and non-list elements in tree
    non-lists from left-to-right, lists bottom-to-top"
    (fold-right (l (b r) (if (list? b) (p (tree-fold-right-depth p (list) b (inc depth) inc) r depth) (p b r depth))) r t))

  (define* (tree-fold-reverse-with-depth p r t #:optional (n 1) (inc 1+))
    "procedure:{any:element any:result integer:nesting-depth} any tree integer:depth-init procedure:{integer -> integer} -> any
     like tree-fold-reverse but with additional arguments for keeping information about the depth of currently processed sub-lists.
    experimental."
    (reverse
      (fold
        (l (e r)
          (if (list? e) (p (tree-fold-reverse-with-depth p (list) e (inc n) inc) r n) (p e r n)))
        r t)))

  (define (tree-map-leafs proc a)
    "procedure:{any -> any} list -> list
     apply proc only with non-list elements, skipping but keeping list-type elements"
    (map (l (e) (if (list? e) (tree-map-leafs proc e) (proc e))) a))

  (define (tree-map-with-depth->flat-list proc a . initial-depth)
    "procedure:{integer:nesting-depth any:element -> any:result-element} list [integer] -> (any ...)
     map elements of tree to a flat list. apply proc with a number
     stating how deeply the element is nested in other lists and the current element"
    (reverse
      (let loop ((rest a) (depth (if (null? initial-depth) 0 (first initial-depth))) (r (list)))
        (if (null? rest) r
          (loop (tail rest) depth
            (if (list? (first rest)) (loop (first rest) (+ 1 depth) r)
              (pair (proc depth (first rest)) r)))))))

  (define (tree-produce proc a b)
    "procedure:{any any -> any} list list -> any
     apply proc with every possible ordered combination of elements between two lists. iterates like tree-map"
    (tree-map (l (e-1) (tree-map (l (e-2) (proc e-1 e-2)) b)) a))

  (define (tree-produce-lists proc a b)
    "like tree-produce but pass only combinations of contained lists to proc"
    (tree-map-lists (l (e-1) (tree-map-lists (l (e-2) (proc e-1 e-2)) b)) a))

  (define (produce-with-iterator-tree iterator proc a b)
    "procedure:{proc list:elements -> any} procedure:{any:element-a any:element-b -> any}:proc list list -> any
     call proc with each ordered combination between elements of two lists with an
     iterator procedure that is called in a nested (each (lambda (e-1) (each (lambda (e-2) (proc e-1 e-2)) b)) a)
     way to create the argument combinations"
    (iterator
      (l (e-1)
        (if (list? e-1) (produce-with-iterator-tree iterator proc e-1 b)
          (iterator
            (l (e-2)
              (if (list? e-2) (produce-with-iterator-tree iterator proc e-1 e-2) (proc e-1 e-2)))
            b)))
      a))

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

  (define (prefix-tree-replace-prefix a replacements)
    "list ((to-replace . replacement) ...) -> list
     replace all list prefixes, the first element of a list, in tree based on the given replacements structure"
    (tree-map
      (l (a depth)
        (if (list? a)
          (let ((replacement (alist-ref replacements (first a))))
            (if replacement (if (procedure? replacement) (replacement a) replacement) a))
          a))
      a))

  (define (prefix-tree-produce proc a)
    "{any ... -> any} list:prefix-tree -> list
     calls proc for each combination of prefix and tail"
    (prefix-tree-produce-with-context (l (context a) (apply proc (reverse (pair context a)))) a))

  (define (prefix-tree->paths a)
    "list -> (string ...)
     regard tree as a nested list representation of a filesystem file and directory structure
     and return a flat list of filesystem path strings.
     example:
     (prefix-tree->paths (list \"/usr\" (list \"bin\" (list \"share\" \"guile\") \"include\") \"/var\"))
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
    (prefix-tree-produce-with-context (l (context a) (reverse (pair context a))) a ignore-prefixes?))

  (define (prefix-tree-product-mm a) "list -> list"
    "like prefix-tree-product, but also interprets lists as prefixes as many-to-many relations
    (many prefixes to tail elements).
    example ((a b) c) -> ((a c) (b c))"
    (prefix-tree-produce-with-context-mm (l (context a) (reverse (pair context a))) a))

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

  (define* (tree-map-with-depth proc a #:optional (initial-depth 0))
    "procedure:{any:element integer:nesting-depth} list [integer] -> list
     like tree-map but with additional arguments for the current nesting-depth"
    (let loop ((rest a) (depth initial-depth) (r (list)))
      (if (null? rest) (proc (reverse r) depth)
        (loop (tail rest) depth
          (pair
            (if (list? (first rest)) (loop (first rest) (+ 1 depth) (list))
              (proc (first rest) depth))
            r)))))

  (define* (tree-map-lists-with-depth proc a #:optional (depth-init 1) (map-depth 1+))
    "{list integer:current-nesting-depth -> any} list [integer {integer -> integer:next-nesting-depth-value}] -> list
     like tree-map-lists with additional arguments for the current nesting-depth"
    (let loop ((e a) (depth depth-init))
      (map (l (e) (if (list? e) (proc (loop e (map-depth depth)) depth) e)) e)))

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
          (map (l (b) (if (list? b) (prefix-tree-map proc b) b)) (tail a))))))

  (define* (prefix-tree-map-with-depth proc a #:optional (initial-depth 0))
    "{any:prefix list:tail integer:nesting-depth -> any} list [integer] -> list
     like prefix-tree-map but with an additional argument for the current nesting-depth"
    (let loop ((rest a) (depth initial-depth))
      (if (null? rest) rest
        (let (prefix (first rest))
          (proc (if (list? prefix) (loop prefix (+ depth 1)) prefix)
            (map (l (e) (if (list? e) (loop e (+ depth 1)) e)) (tail rest)) depth)))))

  (define (produce-prefix-trees proc a b)
    "{any:prefix-1 any:prefix-2 any:tail-1 any:tail-2 -> any} list list -> list
     produce two prefix-trees. calls proc for each ordered combination of
     prefix and tail element of \"a\" and \"b\".
     nested application of prefix-tree-map and all arguments passed to proc"
    (prefix-tree-map
      (l (prefix-1 tail-1)
        (prefix-tree-map (l (prefix-2 tail-2) (proc prefix-1 prefix-2 tail-1 tail-2)) b))
      a))

  (define (produce-prefix-trees-with-depth proc a b)
    "{any:prefix-1 any:prefix-2 any:tail-1 any:tail-2 integer:depth-1 integer:depth-2 -> any} list list -> list
     like produce-prefix-trees but with an argument for the current nesting-depth"
    (prefix-tree-map-with-depth
      (l (prefix-1 tail-1 depth-1)
        (prefix-tree-map-with-depth
          (l (prefix-2 tail-2 depth-2) (proc prefix-1 prefix-2 tail-1 tail-2 depth-1 depth-2)) b))
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
    (prefix-tree-map-with-continue-with-depth proc continue a #:optional (depth-proc 1+)
      (depth-init 1))
    "{any:prefix list:tail any} {list procedure:proc {list ->} any}:continue list [{integer -> integer}] -> list
     like prefix-tree-map-with-continue but with additional arguments for the current nesting-depth"
    (let loop ((rest a) (depth depth-init))
      (if (null? rest) rest
        (proc (first rest)
          (map (l (a) (if (list? a) (continue a proc loop (depth-proc depth)) a)) (tail rest)) depth))))

  (define prefix-tree-map-with-context
    (letrec
      ( (loop
          (l (proc a context) "{any list:parent-prefixes -> any} list list:initial-context -> list"
            (if (null? a) a
              (proc (first a)
                (map (l (b) (if (list? b) (loop proc b (pair (first a) context)) b)) (tail a))
                context)))))
      (l (proc a)
        "{any list:upper-prefixes} list -> list
        like (prefix-tree-map) but with an additional argument for parent prefixes"
        (loop proc a (list)))))

  (define* (prefix-tree-produce-with-context proc a #:optional ignore-prefixes)
    "{element context -> any} list -> list
     context is a list containing nested-list prefixes in bottom-to-top order.
     for example (a (d e f) k (g h i) j) leads to proc called with each of the following arguments:
     (d (a)), (e (d a)), (f (d a)),(k (a)), (g (a)), (h (g a)), (i (g a)), (j (a))
     ignore-prefixes ignores lists that are themselves prefixes"
    (if (null? a) a
      (let*
        ( (map-to-result (l (b prefix context r) (pair (proc b (pair prefix context)) r)))
          (map-to-result-list (if ignore-prefixes (l (b prefix context r) r) map-to-result)))
        (reverse
          (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (r (list)))
            (if (null? rest) r
              (loop (tail rest) prefix
                context
                (let (b (first rest))
                  (if (and (list? b) (not (null? b)))
                    (let (prefix-next (first b))
                      (loop (tail b) prefix-next
                        (pair prefix context) (map-to-result-list prefix-next prefix context r)))
                    (map-to-result b prefix context r))))))))))

  (define (prefix-tree-produce-with-context-mm proc a)
    "procedure:{any list -> any} list -> list
     like prefix-tree-produce-with-context but creates many-to-many relations from lists as list prefixes"
    (if (null? a) a
      (reverse
        (let loop ((rest (tail a)) (prefix (first a)) (context (list)) (result (list)))
          (if (null? rest) result
            (loop (tail rest) prefix
              context
              (let (b (first rest))
                (if (list? prefix)
                  (fold
                    (if (and (list? b) (not (null? b)))
                      (let ((b-first (first b)) (b-tail (tail b)))
                        (l (prefix result)
                          (append (loop b-tail b-first (pair prefix context) (list)) result)))
                      (l (prefix result) (pair (proc b (pair prefix context)) result)))
                    result prefix)
                  (if (and (list? b) (not (null? b)))
                    (loop (tail b) (first b) (pair prefix context) result)
                    (pair (proc b (pair prefix context)) result))))))))))

  (define* (tree-produce-lists-with-depth proc a b #:optional (depth-proc 1+) (depth-init 1))
    "{any:element-a any:element-b integer:nesting-depth-a integer:nesting-depth-b} list:list-a list:list-b [{integer -> integer} integer] -> list"
    (tree-map-lists-with-depth
      (l (e-1 depth-1)
        (tree-map-lists-with-depth (l (e-2 depth-2) (proc e-1 e-2 depth-1 depth-2)) b
          depth-proc depth-init))
      a depth-proc depth-init))

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

  (define (tree-contains-some? a equal-proc . search-values)
    "list {any any -> boolean} any ... -> boolean
     like tree-contains? and true if any of the search-values is found"
    (any (l (search-value) (tree-contains? a search-value equal-proc)) search-values))

  (define (tree-contains-all? a equal-proc . search-values)
    "list {any any -> boolean} any ... -> boolean
     like tree-contains? but true only if every of the search-values has been found"
    (every (l (search-value) (tree-contains? a search-value equal-proc)) search-values))

  (define* (tree-contains-some-not? a equal-proc . search-values)
    "list {any any -> boolean} any ... -> boolean
     like tree-contains? but true only if any of the search-values is not contained"
    (any
      (l (e)
        (if (list? e) (apply tree-contains-some-not? e search-values)
          (not (contains? search-values e equal-proc))))
      a))

  (define (tree->denoted-tree a . initial-depth)
    "list [integer] -> list
     convert a tree to an association list where each element is a list having a nesting-depth number
     as the first element. similar to this is the usage of indentation for nesting depth.
     (a b (c (d e)) f) -> ((0 . a) (0 . b) (1 . c) (2 . d) (2 . e) (0 . f))"
    (apply tree-map-with-depth->flat-list pair a initial-depth))

  (define (denoted-tree-adjust-depth a operator . arguments)
    "procedure list integer -> list
     map the nesting-depth of each element with operator and arguments.
     the lowest possible depth is bounded to zero.
     example:
     (denoted-tree-adjust-depth a + 1)"
    (map (l (a) (pair (max 0 (apply operator (first a) arguments)) (tail a))) a))

  (define (denoted-tree-minimise-depth a)
    "procedure list integer -> list
     decrease nesting-depth for all element until at least one element has a nesting-depth of zero"
    (if (null? a) a
      (let (min-depth (apply min (map first a)))
        (if (zero? min-depth) a (denoted-tree-adjust-depth a - min-depth)))))

  (define (prefix-tree-map-with-depth->flat-list proc a . initial-depth)
    "{integer any -> any} list [integer] -> (any ...)
     like tree->denoted-tree->flat-list but the nesting-depth number corresponds to prefix-tree interpretation"
    (reverse
      (let loop
        ( (rest a) (depth (if (null? initial-depth) 0 (first initial-depth))) (is-prefix #t)
          (r (list)))
        (if (null? rest) r
          (loop (tail rest) depth
            #f
            (if (list? (first rest)) (loop (first rest) (+ 1 depth) #t r)
              (pair
                (proc (if (and is-prefix (> depth 0) (not (null? (tail rest)))) (- depth 1) depth)
                  (first rest))
                r)))))))

  (define (prefix-tree->denoted-tree a . initial-depth)
    "list [integer] -> list
     like tree->denoted-tree but the nesting-depth number corresponds to prefix-tree interpretation. example
     (a b (c (d e)) f) -> ((0 . a) (1 . b) (2 . c) (3 . d) (3 . e) (0 . 4))"
    (apply prefix-tree-map-with-depth->flat-list pair a initial-depth))

  (define (tree-filter->flat-list predicate a)
    "procedure:{any -> boolean} list -> list
     results in a flat list of all elements (non-lists and lists) of tree that match predicate"
    (let loop ((rest a) (r (list)))
      (if (null? rest) r
        (loop (tail rest)
          (let (b (first rest)) (if (predicate b) (pair b r) (if (list? b) (loop b r) r)))))))

  (define (tree-pair->list a)
    "list -> list
     converts all non-list pairs in tree to lists"
    (tree-map (l (e) (if (pair? e) (list (first e) (tail e)) e)) a)))
