(library (sph documentation display-format-plist)
  (export
    display-format-plist)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph binding-info)
    (sph alist)
    (sph documentation)
    (only (srfi srfi-1) remove))

  (define-as display-format-plist alist-quoted
    format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments)
      (pair (symbol->string (bi-name bi))
        (append
          (itpn-docstring-split-signature (bi-documentation bi)
            (l (signature text-lines)
              (append
                (if signature
                  (list
                    (pair "signature"
                      (if (string-null? formatted-arguments) (list signature)
                        (list formatted-arguments signature))))
                  (if (string-null? formatted-arguments) (list)
                    (list (list "signature" formatted-arguments))))
                (if (null? text-lines) text-lines
                  (list (list "description" (string-join (remove string-null? text-lines) "\n")))))))
          (list (list "type" (symbol->string (bi-type bi)))))))
    format-module-documentation (l (module-name md) md) format-modules-documentation (l (mds) mds))

  (set! documentation-display-formats
    (pair (pair (q plist) display-format-plist) documentation-display-formats)))
