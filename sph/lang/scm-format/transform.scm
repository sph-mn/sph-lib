(library (sph lang scm-format transform)
  (export
    is-library?
    scm-format-transform-tree)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (rnrs lists)
    (rnrs sorting)
    (sph)
    (sph hashtable)
    (sph list)
    (sph string))

  (define (definition? arg)
    (and (list? arg) (not (null? arg))
      (symbol? (first arg)) (string-prefix? "define" (symbol->string (first arg)))))

  (define (export-ele->string arg)
    (string-join (map (l (ele) (if (list? ele) (export-ele->string ele) (symbol->string ele))) arg) ""))

  (define-syntax-rule (export-ele<? a b) (string< (export-ele->string a) (export-ele->string b)))

  (define (export-name< a b)
    (if (any list? a) (if (any list? b) (export-ele<? a b) #f) (if (any list? b) #t (export-ele<? a b))))

  (define (get-definition-name arg) (match arg ((_ (name . _) _ ...) name) ((_ name _ ...) name) (_ #f)))

  (define (separate-unexported-definitions arg exports sort? after?)
    (split-definitions arg
      (l (definitions rest)
        (call-with-values
          (l ()
            (partition (l (ele) (contains? exports (get-definition-name ele)))
              (if sort? (sort-definitions definitions) definitions)))
          (l (exported unexported)
            (if after? (append exported unexported rest) (append unexported exported rest)))))))

  (define (sort-definitions arg)
    (list-sort
      (l (a b) (string< (symbol->string (get-definition-name a)) (symbol->string (get-definition-name b)))) arg))

  (define (sort-import-export-list arg)
    (list-sort
      (l (a b)
        (if (symbol? a) (if (symbol? b) (string< (symbol->string a) (symbol->string b)) #t)
          (if (symbol? b) #f (export-name< a b))))
      arg))

  (define (split-definitions arg proc) (call-with-values (l () (partition definition? arg)) proc))

  (define (transform-library arg config)
    (match arg
      ( (_ name (_ export ...) (_ import ...) body ...)
        (pairs (q library) name
          (pair (q export) (if (hashtable-ref config (q sort-export)) (sort-import-export-list export) export))
          (pair (q import) (if (hashtable-ref config (q sort-import)) (sort-import-export-list import) import))
          (let
            (config-separate-unexported-definitions (hashtable-ref config (q separate-unexported-definitions)))
            (if config-separate-unexported-definitions
              (separate-unexported-definitions body export
                (hashtable-ref config (q sort-definitions))
                (eq? (q after) config-separate-unexported-definitions))
              (if (hashtable-ref config (q sort-definitions))
                (split-definitions body (l (definitions rest) (append (sort-definitions definitions) rest))) body)))))
      (_ arg)))

  (define (is-library? arg) (and (list? arg) (not (null? arg)) (eq? (q library) (first arg))))

  (define (scm-format-transform-tree arg config) "toplevel-exprs config-transform"
    (map (l (ele) (if (is-library? ele) (transform-library ele config) ele)) arg)))