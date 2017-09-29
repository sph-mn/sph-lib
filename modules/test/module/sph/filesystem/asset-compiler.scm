(define-test-module (test module sph filesystem asset-compiler)
  (import
    (sph base)
    (sph record)
    (sph filesystem asset-compiler))

  (define-test (ac-destination)
    (and
      (let (r (ac-destination "/tmp" (q sxml) (list "test") #f ".sxml"))
        (and (string? r) (string-prefix? "/tmp" r)))
      (let (r (ac-destination "/tmp" (q sxml) (list "test") "xyz" ".sxml"))
        (and (string? r) (string-prefix? "/tmp" r) (string-suffix? "xyz.sxml" r)))))

  (test-execute-procedures-lambda (ac-destination)))
