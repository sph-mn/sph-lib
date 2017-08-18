(define-test-module (test module sph hashtable)
  (import
    (sph alist)
    (sph list)
    (sph hashtable))

  (define data-1 (ht-create-symbol a 1 b (ht-create-symbol c 2)))
  (define data-2 (ht-create-symbol d 3 b (ht-create-symbol c 4 e 5) f 6))

  (define-test (ht-copy*)
    (let* ((data (ht-copy data-1 #t)) (r (ht-copy* data (l (a) (ht-set! a (q a) 2)))))
      (ht-set! r (q a) 3)
      (assert-true
        (and (= 1 (ht-ref data (q a))) (= 1 (ht-ref data-1 (q a))) (ht? r) (= 3 (ht-ref r (q a)))))))

  (define-test (ht-alist)
    (let (r (ht-alist data-1))
      (assert-true (and (list? r) (not (null? r)) (every pair? r) (= (length r) (ht-size data-1))))))

  (define-test (ht-from-alist)
    (let (a (ht-from-alist (ht-alist data-1)))
      (assert-true (= 1 (ht-ref-q a a)) (= 2 (ht-tree-ref-q a b c)))))

  (define (alist->list a)
    (fold-right (l (a r) (pairs (first a) (let (b (tail a)) (if (alist? b) (alist->list b) b)) r))
      (list) a))

  (define-test (ht-tree-merge!)
    (let
      (r
        (flatten
          (alist->list (ht-alist (ht-tree-copy* data-1 (l (a) (ht-tree-merge! a data-2))) (inf)))))
      (assert-true (list-set-equal? r (q (a 1 f 6 b e 5 c 4 d 3))))))

  (test-execute-procedures-lambda ht-tree-merge! ht-copy* ht-from-alist ht-alist))
