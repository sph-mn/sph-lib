(define-test-module (test module sph list)
  (import
    (sph list))

  (define-test (env-replace-at-once-proc a)
    (map (l (e) (* 3 e)) a))

  (define-test (group-split-at-matches arguments)
    (group-split-at-matches integer? arguments))

  (test-execute-procedures-lambda
    (flatten
      ((1 (2 3 (4) (5 (6 7)) 8)))
      (1 2 3 4 5 6 7 8))
    (group-split-at-matches
      ("a" "b" 1 "c" "d" 2 "e")
      (("a" "b") (1 "c" "d") (2 "e"))
      () ()
      (1 2)
      ((1) (2))
      (1 "c" "d" 2)
      ((1 "c" "d") (2)))
    (intersection
      ((1 2 3) (5 6 2 8)) (2)
      ((1 2 3) (4 5 6)) ()
      (()) ()
      ((1 2 2 3) (4 2 2 5))
      (2 2))
    (difference
      ((1 2 3) (5 6 2 8)) (5 6 8 1 3)
      ((1 2 3) (4 5 6)) (4 5 6 1 2 3)
      (() ()) ()
      ((1 2 2 3 3) (4 2 2 5)) (4 5 1 3 3)
      ((1 2) (1 2)) ())
    (replace-at-once
      ;matches in the middle, modification of matches
      ((unquote even?) (unquote test-env-replace-at-once-proc) (1 2 3 4 5))
      (1 6 3 12 5)
      ;matches at beginning and end
      ((unquote odd?) (unquote test-env-replace-at-once-proc) (1 2 3 4 5))
      (3 2 9 4 15))
    (complement
      ((1 2 3) (5 6 2 8)) (1 3)
      ((1 2 3) (4 5 6)) (1 2 3)
      (() ()) ()
      ((1 2 2 3 3) (4 2 2 5)) (1 3 3)
      ((1 2) (1 2)) ()
      ((2 1 3 4 5) (3 1 2 5 4 2)) ()
      ((2 1 3 4 5) (3 2 5 4 2)) (1))
    (interleave
      ((a b c d) 1) (a 1 b 1 c 1 d)
      (() 1) ()
      ((a) 1) (a))
    (split-by-pattern
      ;no ellipsis
      ((a b c) (1 2 3 4 5 6 7)) (((a . 1) (b . 2) (c . 3)) (4 5 6 7))
      ;middle ellipsis
      ((a b c ... d e) (1 2 3 4 5 6 7)) (((a . 1) (b . 2) (c 3 4 5) (d . 6) (e . 7)) ())
      ;trailing ellipsis
      ((a b c ...) (1 2 3 4 5 6 7)) (((a . 1) (b . 2) (c 3 4 5 6 7)) ())
      ((a b ...) (1 2))  (((a . 1) (b 2)) ())
      ;honoring the ellipsis as optional indicator
      ((a b c ...) (1 2)) (((a . 1) (b . 2)) ())
      ((a b ... c) (1 2)) (#f #f))
    (pattern-match-min-length
      ((a ...)) 0
      ((a a ...)) 1
      ((a ... b ...)) 0
      ((a ... b ... c d)) 2
      ((a ... e b ... c d)) 3)
    (flat?
      ((1 2 3))
      #t
      ((1 2 (3 4)))
      #f
      ((1 (2 3) 4))
      #f
      (((1 2) 3 4))
      #f
      (((1 2) (3 4)))
      #f
      (((1 2) 3 (4 5)))
      #f)
    (count-value
      (1 (2 1 1))
      2)
    (delete-duplicates-sorted
      ((1 2 3 3 4 5 5 5))
      (1 2 3 4 5)
      ((1 2 3 3 4 5 5 5) ,eq? #f)
      (5 4 3 2 1))
    (group-consecutive
      ((unquote even?) (1 2 2 3 4 4 6 5 6 7))
      (1 (2 2) 3 (4 4 6) 5 6 7))
    (fold-multiple-c
      ((unquote
          (lambda (e continue a b)
            (continue (pair e a) (+ b 1))))
        (5 4 3 2 1) () 0)
      ((1 2 3 4 5) 5)
      ((unquote
          (lambda (e continue a b)
            (if (< b e)
              (continue (pair e a) (+ b 1))
              a)))
        (5 4 3 2 1) () 0)
      (3 4 5))
    (list-select
      ((1 2 3 4 5) (1 3 4))
      (2 4 5))
    (list-logical-contains?
      ((1 2 3) (or 5 4 3 6)) #t
      ((1 2 3) (or 5 4 6)) #f
      ((1 2 3) (and 2 3 4)) #f
      ((1 2 3) (and 2 3)) #t
      ((1 2 3) (or (and 2 4) (and 2 3))) #t
      ((1 2 3) (or (and 2 4) (and 2 5))) #f
      ((a b) (and b c)) #f)
    (list-logical-condition?
      ((and 3 4)) #t
      ((andx 3 4)) #f)
    (list-index-value
      ((1 2 3) 2) 1
      ((1 2 3) 2 (unquote eqv?)) 1)
    (list-sort-by-list
      ((3 2 4) (4 2 3)) (3 2 4)
      ((3 2 4) (4 5 2 3)) (3 2 4 5)
      ((3 2 4) ((4) (2) (3)) (unquote first)) ((3) (2) (4)))
    (map-consecutive
      ((unquote even?) (unquote (l args (map 1+ args))) (1 2 2 3 4 4 6 5 6 7))
      (1 (3 3) 3 (5 5 7) 5 6 7)
      ((unquote even?) (unquote (l args (map 1+ args))) (2 2 3 4 4 6 5 6 6))
      ((3 3) 3 (5 5 7) 5 (7 7)))
    (map-segments
      (3 (unquote list) (1 2 3 4 5))
      ((1 2 3) (2 3 4) (3 4 5))
      (3 (unquote list) (1 2))
      ((1 2))
      (1 (unquote list) (1 2))
      ((1) (2))
      (3 (unquote list) ())
      ())
    (produce
      ((unquote +) (1 2 3) (3 4))
      (4 5 5 6 6 7)
      ((unquote list) (1 2) (3) (4 5))
      ((1 3 4) (1 3 5) (2 3 4) (2 3 5))
      (,+ (1 3) (2 6)) (3 7 5 9)
      (,+ (1 3) (6)) (7 9))
    (contains?
      ((1 2 3) 3)
      #t
      ((1 2 3) 4)
      #f)
    (contains-all?
      ((1 2 3) (2 3)) #t
      ((1 2 3) (1)) #t
      ((1 2 3) ()) #t
      ((1 2 3) (2 3 4)) #f)
    (contains-some?
      ((1 2 3) (4 3)) #t
      ((1 2 3) (1 4)) #t
      ((1 2 3) ()) #f
      ((1 2 3) (2 3 4)) #t)
    (every-map
      ((unquote identity) (1 2 3)) (1 2 3)
      ((unquote odd?) (1 2 3)) #f)
    (list-replace-last
      ((1 2 3) 4) (1 2 4))
    (list-replace-last-n
      (2 (1 2 3) 4) (1 4))
    (map-last-n
      (2 (1 2 3) (unquote (l (a b) (list 4 5))) )
      (1 4 5))
    (simplify
      ((1)) 1
      ((1 2)) (1 2)
      ((1 2 3)) (1 2 3))
    (convolve
      ((1 2 3) (0 1 0.5))
      (0 1 2.5 4.0 1.5)
      ((0 -1 -2 2 1 1 0 0 6) (1 -1 0 -2))
      (0 -1 -1 4 1 4 -5 -2 4 -6 0 -12))
    (duplicates
      ((2 1 1 1 3 3 4)) (1 3)
      (()) ()
      ((1 2 3 4)) ())
))
