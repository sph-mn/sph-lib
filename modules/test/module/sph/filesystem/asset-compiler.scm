(define-test-module (test module sph filesystem asset-compiler)
  (import
    (sph base)
    (sph record)
    (sph filesystem asset-compiler))

  (define proc (l () #t))

  (define valid-config
    (ht-create-symbol javascript (list proc (vector (q sescript) proc proc))
      html (list #f (vector (q sxml) proc proc))
      css (list proc (vector (q plcss) proc proc) (vector (q css) proc proc))))

  (define invalid-config-1
    (ht-create-symbol javascript (list proc (vector (q sescript) proc proc))
      css (list proc (vector (q plcss) proc))))

  (define invalid-config-2 (ht-create-symbol css (list (vector (q plcss) proc proc))))
  (define invalid-config-3 (ht-create-symbol css (list (vector) (vector (q plcss) proc proc))))
  (define invalid-config-4 (vector))

  (define-test (ac-destination)
    (and
      (let (r (ac-destination "/tmp" (q sxml) (list "test")))
        (and (string? r) (string-prefix? "/tmp" r)))
      (let (r (ac-destination "/tmp" (q sxml) (list "test") "xyz"))
        (and (string? r) (string-prefix? "/tmp" r) (string-suffix? "xyz" r)))))

  (test-execute-procedures-lambda
    (ac-config-valid? (unquote valid-config) #t
      (unquote invalid-config-1) #f
      (unquote invalid-config-2) #f (unquote invalid-config-3) #f (unquote invalid-config-4) #f)
    (ac-destination)))
