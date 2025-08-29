(define-test-module (test module sph tree)
  (import
    (sph tree))

  (define data-tree (list 1 2 3 (list 4 5 (list 6 7) (list 8 9 10))))

  (test-execute-procedures-lambda
    (tree-find
      (unquote (list(l (a) (or (list? a) (= 6 a))) data-tree))
      (4 5 (6 7) (8 9 10)))
    (tree-any
      (unquote (list (l (a) (and (integer? a) (= 6 a) a)) data-tree))
      6)
    (tree-fold-right
      (unquote (list (l (a r) (pair a r)) null data-tree))
      (unquote data-tree)
      (unquote (list (l (a r) (if (list? a) a (pair (even? a) r))) null data-tree))
      (#f #t #f #t #f #t #f))
    (tree-fold-depth
      (unquote (list (l (a r d) (if (list? a) (pair a r) (pair d r))) null data-tree))
      (((3 3 3) (3 3) 2 2) 1 1 1))
    (tree-filter
      (unquote (list (l (a) (or (list? a) (even? a))) data-tree))
      (2 (4 (6) (8 10))))
    (tree-filter-lists
      (unquote (list (l (a) (< 3 (length a))) data-tree))
      (1 2 3))
    (tree-map-leafs
      (unquote (list even? data-tree))
      (#f #t #f (#t #f (#t #f) (#t #f #t))))
    (denoted-tree->prefix-tree
      (((0 . a) (1 . b) (2 . c))) ((a (b c)))
      (((3 . b) (3 . c) (0 . d)))
      ((((b c))) d)
      (((1 . a) (3 . b) (3 . c) (0 . d)))
      (((a (b c))) d)
      (((1 . a) (4 . b) (4 . c) (0 . d)))
      (((a ((b c)))) d)
      (((0 . a) (1 . b) (1 . c) (2 . d) (1 . e) (0 . f) (1 . g)))
      ((a b (c d) e) (f g))
      (((0 . a) (1 . b) (2 . c) (1 . d) (2 . e) (3 . f) (0 . g)))
      ((a (b c) (d (e f))) g)
      (((0 . a) (2 . b) (0 . c)))
      ((a (b)) c)
      (((0 . a) (2 . b) (1 . c)))
      ((a (b) c)))
    (prefix-tree->denoted-tree
      ((a (b (c d))))
      ((0 . a) (0 . b) (1 . c) (2 . d)))
    (prefix-tree-produce-with-context
      ((unquote list) (a (d e f) (g h i) j) #t)
      ((e (d a)) (f (d a)) (h (g a)) (i (g a)) (j (a)))
      ((unquote list) (a b (b (c (d e) f))) #t)
      ((b (a)) (e (d c b a)) (f (c b a)))
      ((unquote list) ((a b) (c d)) #t)
      ((d (c (a b))))
      ((unquote list) () #t) ()
      ((unquote list) (a (c () b)) #t) ((() (c a)) (b (c a)))
      ((unquote list) (a c d) #t)
      ((c (a)) (d (a)))
      ((unquote list) (a (d e f) (g h i) j) #f)
      ((d (a)) (e (d a)) (f (d a)) (g (a)) (h (g a)) (i (g a)) (j (a)))
      ((unquote list) (a b (b (c (d e) f))) #f)
      ((b (a)) (b (a)) (c (b a)) (d (c b a)) (e (d c b a)) (f (c b a)))
      ((unquote list) ((a b) (c d)) #f)
      ((c ((a b))) (d (c (a b))))
      ((unquote list) () #f) ()
      ((unquote list) (a c d) #f)
      ((c (a)) (d (a))))
    (prefix-tree-produce-with-context-mm
      ((unquote list) ((a b) c d))
      ((c (a)) (c (b)) (d (a)) (d (b)))
      ((unquote list) ((d e) ((a b) c)))
      ((c (a d)) (c (b d)) (c (a e)) (c (b e)))
      ((unquote list) (a (c d)))
      ((d (c a)))
      ((unquote list) (a ((c d) e)))
      ((e (c a)) (e (d a)))
      ((unquote list) ()) ()
      ((unquote list) (a (c () b)))
      ((() (c a)) (b (c a))))
    (prefix-tree-product
      ((1 2 3 4) #t)
      ((1 2) (1 3) (1 4))
      ((1 2 (3 (4 5 6) 7) (8 9)) #t)
      ((1 2) (1 3 4 5) (1 3 4 6) (1 3 7) (1 8 9))
      (()) ()
      ((1) #t) ())
    (prefix-tree-product-mm
      (((1 2) 3 4))
      ((1 3) (2 3) (1 4) (2 4))
      (((1 2) (3 (4 5))))
      ((1 3 4 5) (2 3 4 5))
      ((1 ((2 3) (4 5))))
      ((1 2 4 5) (1 3 4 5)))
    (denoted-tree->tree
      (((0 . a) (0 . b) (1 . c) (2 . d) (2 . e) (1 . f) (0 . g)))
      (a b (c (d e) f) g)
      ;depth skip at the end
      (((0 . a) (1 . b) (2 . c) (0 . d)))
      (a (b (c)) d)
      ;ending with a non-zero depth
      (((0 . a) (1 . b) (2 . c)))
      (a (b (c)))
      ;starting with a non-zero depth
      (((2 . a) (0 . b) (1 . c)))
      (((a)) b (c))
      (((0 . a) (1 . b) (1 . c) (2 . d) (1 . e) (0 . f) (1 . g)))
      (a (b c (d) e) f (g)))))
