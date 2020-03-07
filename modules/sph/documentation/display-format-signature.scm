(define-module (sph documentation display-format-signature))

(use-modules (sph) (sph alist)
  (sph documentation) (sph list) (sph module binding-info) (srfi srfi-1))

(export display-format-signature sph-documentation-display-format-signature-description)

(define sph-documentation-display-format-signature-description
  "only names and type signatures.
   currently it displays argument names as defined in the procedure, not type names from signatures as the first line the docstring")

(define display-format-signature
  (alist-q format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (docstring-split-signature (bi-documentation bi) ""
        (l (signature text-lines)
          (let
            ( (arguments-string formatted-arguments)
              (docstring
                (string-join (remove string-null? (if text-lines (any->list text-lines) null))
                  "\n  " (q prefix))))
            (string-append (symbol->string (bi-name bi))
              (if (contains? (list-q procedure syntax) (bi-type bi))
                (string-append " :: " arguments-string) ""))))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n"))
    format-modules-documentation
    (l (mds) "(string ...) -> string" (string-append (apply string-append mds) "\n"))))

(set! documentation-display-formats
  (pair (pair (q signature) display-format-signature) documentation-display-formats))
