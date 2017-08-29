(define-test-module (test module sph number)
  (import
    (sph number)
    (sph list))

  (define-test (container-length->number-max arguments)
    ;arguments: (range base)
    (map-integers (first arguments) (l (n) (container-length->number-max n (second arguments)))))

  (test-execute-procedures-lambda
    (number-container-length (1383213160 2) 31
      (1 2) 1 (0 2) 0 (8 2) 4 (255 2) 8 (256 2) 9 (9 10) 1 (11 10) 2 (16 16) 2 (15 16) 1)
    (container-length->number-max (10 2) (0 1 3 7 15 31 63 127 255 511)
      (10 12) (0 11 143 1727 20735 248831 2985983 35831807 429981695 5159780351))
    (round-to-increment (1 3) 0 (0 3) 0 (1.4 3) 0.0 (1.5 3) 0.0 (3 3) 3 (3.1 3) 3.0 (2 4) 0)
    (percent (3 200) 3/2 (200 200) 100 (0 200) 0)
    (round-to-decimal-places (3.7324 2) 3.73 (3.7355 2) 3.74)
    (truncate-to-decimal-places (3.7324 2) 3.73 (3.7355 2) 3.73 (3.735523402883043 2) 3.73)
    (integer-and-fraction& (3.732 (unquote list)) (3 0.732)) (average (1 2 3 4) 5/2)
    (absolute-difference (1 3) 2 (3 1) 2 (-1 -3) 2 (-3 -1) 2 (3 -1) 4 (-1 3) 4)
    (in-between? (3 4 5) #f (4 3 5) #t (4 2 7) #t (0 0 1) #f (2 4 1) #f)))
