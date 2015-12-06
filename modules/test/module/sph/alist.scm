(define-test-module (test module sph alist)
  (import
    (sph alist))

  (define-tests tests
    (list->alist
      ((#f #t #t #f #f #f))
      ((#f . #t) (#t . #f) (#f . #f))
      ((1 2 3))
      ((1 . 2) (3))
      ((1 2 3 4))
      ((1 . 2) (3 . 4)))
    (alist-merge
      (((1 . 2) (3 . 4)) ((5 . 6) (3 . 7)))
      ((1 . 2) (5 . 6) (3 . 7)))
    (alist-update
      (((1 . 2) (3 . 4)) ((3 . 5) (4 . 7)))
      ((1 . 2) (3 . 5))))

  (define (execute settings)
    (test-execute-procedures settings tests)))
