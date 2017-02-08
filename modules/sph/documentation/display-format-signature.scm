(library (sph documentation display-format-signature)
  (export
    display-format-signature)
  (import
    (guile)
    (sph)
    (sph alist)
    (sph binding-info)
    (sph documentation)
    (sph list)
    (only (srfi srfi-1) remove))

  (define-as display-format-signature alist-q
    format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (docstring-split-signature (bi-documentation bi) ""
        (l (signature text-lines)
          (let
            ( (arguments-string
                (if (or signature (not (string-null? formatted-arguments)))
                  (string-append
                    (if (and signature (not (string-contains signature "\n"))) signature
                      (if formatted-arguments formatted-arguments "")))
                  ""))
              (docstring (string-join (remove string-null? text-lines) "\n  " (q prefix))))
            (string-append (symbol->string (bi-name bi))
              (if (contains? (ql procedure syntax) (bi-type bi))
                (string-append " :: " arguments-string) "")
              docstring)))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n"))
    format-modules-documentation
    (l (mds) "(string ...) -> string" (string-append (apply string-append mds) "\n")))

  (set! documentation-display-formats
    (pair (pair (q signature) display-format-signature) documentation-display-formats)))
