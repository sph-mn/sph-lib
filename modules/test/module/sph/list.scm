(define-test-module (test module sph list)
  (import
    (sph list))

  (define-test (env-replace-at-once-proc a)
    (map (l (e) (* 3 e)) a))

  (test-execute-procedures-lambda
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
      ((a ... e b ... c d)) 3
      )
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
    (count-occurence
      (1 (2 1 1))
      2)
    (delete-duplicates-sorted
      ((1 2 3 3 4 5 5 5))
      (1 2 3 4 5)
      ((1 2 3 3 4 5 5 5) ,eq? #f)
      (5 4 3 2 1))
    (group-successive
      ((unquote even?) (1 2 2 3 4 4 6 5 6 7))
      (1 (2 2) 3 (4 4 6) 5 6 7))
    (fold-multiple-with-continue
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
    (list-set-match-contains?
      ((1 2 3) (some 5 4 3 6)) #t
      ((1 2 3) (some 5 4 6)) #f
      ((1 2 3) (all 2 3 4)) #f
      ((1 2 3) (all 2 3)) #t
      ((1 2 3) (some (all 2 4) (all 2 3))) #t
      ((1 2 3) (some (all 2 4) (all 2 5))) #f
      ((a b) (all b c)) #f)
    (list-set-match-condition?
      ((some 3 4)) #t
      ((somex 3 4)) #f)
    (list-sort-by-list
      ((3 2 4) (4 2 3)) (3 2 4)
      ((3 2 4) (4 5 2 3)) (3 2 4 5))
    (map-successive
      ((unquote even?) (unquote (l args (map 1+ args))) (1 2 2 3 4 4 6 5 6 7))
      (1 (3 3) 3 (5 5 7) 5 6 7)
      ((unquote even?) (unquote (l args (map 1+ args))) (2 2 3 4 4 6 5 6 6))
      ((3 3) 3 (5 5 7) 5 (7 7)))
    (map-segments
      ((unquote list) 3 (1 2 3 4 5))
      ((1 2 3) (2 3 4) (3 4 5))
      ((unquote list) 3 (1 2))
      ((1 2))
      ((unquote list) 1 (1 2))
      ((1) (2))
      ((unquote list) 3 ())
      ())
    (produce
      ((unquote +) (1 2 3) (3 4))
      (4 5 5 6 6 7)
      ((unquote list) (1 2) 3 (4 5))
      ((1 3 4) (1 3 5) (2 3 4) (2 3 5))
      (,+ (1 3) (2 6)) (3 7 5 9)
      (,+ (1 3) (6)) (7 9))
    (produce-controlled
      ((unquote list) ((unquote map) (unquote map)) (1 2) (3 4))
      (((1 3) (1 4)) ((2 3) (2 4))))
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
    (every-map
      ((unquote identity) (1 2 3)) (1 2 3)
      ((unquote odd?) (1 2 3)) #f)
    (list-replace-last
      ((1 2 3) 4) (1 2 4)
      ((1 2 3) (unquote (l (ele) (list 4 5)))) (1 2 4 5)
      ((1 2 3) (unquote (l (ele) (list 4 ele 5)))) (1 2 4 3 5))
    (list-replace-last-n
      (2 (1 2 3) 4) (1 4)
      (2 (1 2 3) (unquote (l (ele ele-2) (list 4 5))))
      (1 4 5))
    (simplify
      ((1)) 1
      ((1 2)) (1 2)
      ((1 2 3)) (1 2 3))))
