;transformations on the abstract syntax tree

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
    (sph string)
    (only (srfi srfi-1) delete-duplicates))

  (define (definition? a)
    (and (list? a) (not (null? a))
      (symbol? (first a)) (string-prefix? "define" (symbol->string (first a)))))

  (define (export-ele->string a)
    (string-join (map (l (e) (if (list? e) (export-ele->string e) (symbol->string e))) a) ""))

  (define-syntax-rule (export-ele<? a b) (string< (export-ele->string a) (export-ele->string b)))

  (define (export-name< a b)
    (if (any list? a) (if (any list? b) (export-ele<? a b) #f)
      (if (any list? b) #t (export-ele<? a b))))

  (define (get-definition-name a)
    (match a ((_ (name . _) _ ...) name) ((_ name _ ...) name) (_ #f)))

  (define (separate-unexported-definitions a exports sort? after?)
    (split-definitions a
      (l (definitions rest)
        (call-with-values
          (l ()
            (partition (l (e) (contains? exports (get-definition-name e)))
              (if sort? (sort-definitions definitions) definitions)))
          (l (exported unexported)
            (if after? (append exported unexported rest) (append unexported exported rest)))))))

  (define (sort-definitions a)
    (list-sort
      (l (a b)
        (string< (symbol->string (get-definition-name a)) (symbol->string (get-definition-name b))))
      a))

  (define (sort-import-export-list a) "list -> list"
    (list-sort
      (l (a b)
        (if (symbol? a) (if (symbol? b) (string< (symbol->string a) (symbol->string b)) #t)
          (if (symbol? b) #f (export-name< a b))))
      a))

  (define (delete-duplicate-import-exports a) "list -> list" (delete-duplicates a))
  (define (split-definitions a proc) (call-with-values (l () (partition definition? a)) proc))

  (define (transform-library a config) "any hashtable -> list/any"
    (match a
      ( (_ name (_ export ...) (_ import ...) body ...)
        (pairs (q library) name
          (pair (q export)
            (delete-duplicate-import-exports
              (if (hashtable-ref config (q sort-export)) (sort-import-export-list export) export)))
          (pair (q import)
            (delete-duplicate-import-exports
              (if (hashtable-ref config (q sort-import)) (sort-import-export-list import) import)))
          (let
            (config-separate-unexported-definitions
              (hashtable-ref config (q separate-unexported-definitions)))
            (if config-separate-unexported-definitions
              (separate-unexported-definitions body export
                (hashtable-ref config (q sort-definitions))
                (eqv? (q after) config-separate-unexported-definitions))
              (if (hashtable-ref config (q sort-definitions))
                (split-definitions body
                  (l (definitions rest) (append (sort-definitions definitions) rest)))
                body)))))
      (_ a)))

  (define (is-library? a) "any -> boolean"
    (and (list? a) (not (null? a)) (eqv? (q library) (first a))))

  (define (scm-format-transform-tree a config) "toplevel-exprs config-transform"
    (map (l (e) (if (is-library? e) (transform-library e config) e)) a)))