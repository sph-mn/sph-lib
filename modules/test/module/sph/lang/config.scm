(define-test-module (test module sph lang config)
  (import
    (sph hashtable)
    (sph alist)
    (sph lang config))

  (define data-config
    "abc #xa de #(1 3 4) f (g 5 h \"h6\" i (.. 7 8) j (k 9)) ll (.. (unquote (+ 1 9)))")

  (define-test (config-read)
    (let*
      ( (a (config-read (open-input-string data-config)))
        (expected-top-level (alist-q abc 10 ll (list 10) de (vector 1 3 4) f ht?))
        (expected-f (alist-q g 5 h "h6" i (list 7 8) j (l (j) (and (ht? j) (= 9 (ht-ref-q j k)))))))
      (every
        (l (result-ht expected)
          (every
            (l (expected)
              (let* ((k (first expected)) (check (tail expected)) (v (ht-ref result-ht k)))
                (or (if (procedure? check) (check v) (equal? check v))
                  (pair (q unexpected-value) k))))
            expected))
        (list a (ht-ref-q a f)) (list expected-top-level expected-f))))

  (test-execute-procedures-lambda config-read))
