(library (sph documentation display-format-plist)
  (export
    display-format-plist)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph module binding-info)
    (sph documentation))

  (define sph-documentation-display-format-plist
    "display format for (sph documentation).
     format that creates a list with entries like this: (dg-any-read (signature _ [_] dg-selection [count] -> error/(vector ...)) (description ) (type procedure))")

  (define-as display-format-plist alist-q
    format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments)
      (pair (symbol->string (bi-name bi))
        (append
          (docstring-split-signature (bi-documentation bi) ""
            (l (signature text)
              (append
                (if signature
                  (list
                    (pair (q signature)
                      (if (string-null? formatted-arguments) (list signature)
                        (list formatted-arguments signature))))
                  (if (string-null? formatted-arguments) (list)
                    (list (list (q signature) formatted-arguments))))
                (if (or (not text) (string-null? text)) null (list (list (q description) text))))))
          (list (list (q type) (bi-type bi))))))
    format-module-documentation (l (module-name md) md) format-modules-documentation (l (mds) mds))

  (set! documentation-display-formats
    (pair (pair (q plist) display-format-plist) documentation-display-formats)))
