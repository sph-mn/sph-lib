(define-test-module (test module sph filesystem asset-compiler)
  (import
    (sph base)
    (sph record)
    (sph filesystem asset-compiler))

  (define proc (l () #t))

  (define valid-config
    (symbol-hashtable javascript
      (list (symbol-hashtable production proc development proc)
        (record ac-config-input (q sescript) proc proc))
      html (list (symbol-hashtable) (record ac-config-input "sxml" proc proc))
      css
      (list (symbol-hashtable production proc development proc)
        (record ac-config-input (q plcss) proc proc))))

  (define invalid-config-1
    (symbol-hashtable javascript
      (list (symbol-hashtable production proc development proc)
        (record ac-config-input (q sescript) proc proc))
      "css"
      (list (symbol-hashtable production proc development proc)
        (record ac-config-input (q plcss) proc proc))))

  (define invalid-config-2
    (symbol-hashtable css (list (record ac-config-input (q plcss) proc proc))))

  (define invalid-config-3
    (symbol-hashtable css (list (vector) (record ac-config-input (q plcss) proc proc))))

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
