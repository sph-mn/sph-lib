(library (sph documentation display-format-itpn)
  (export
    default-format-arguments
    display-format-itpn)
  (import
    (guile)
    (ice-9 peg)
    (sph)
    (sph alist)
    (sph module binding-info)
    (sph documentation)
    (sph lang parser type-signature)
    (sph list)
    (sph string)
    (except (srfi srfi-1) map)
    (only (ice-9 regex) regexp-substitute/global))

  (define itpn-indent (string #\space #\space))

  (define-as display-format-itpn alist-q
    format-arguments default-format-arguments
    format-binding-info
    (l (bi formatted-arguments) "vector:record string -> string"
      (string-append (symbol->string (bi-name bi)) "\n"
        (docstring-split-signature (bi-documentation bi) (string-append itpn-indent itpn-indent)
          (l (signature text-lines)
            (string-append
              (if (or signature (not (string-null? formatted-arguments)))
                (string-append itpn-indent "signature\n"
                  itpn-indent itpn-indent
                  (if (string-null? formatted-arguments) formatted-arguments
                    (string-append formatted-arguments "\n"))
                  (if signature (string-append itpn-indent itpn-indent signature "\n") ""))
                "")
              (lines->docstring text-lines itpn-indent))))
        itpn-indent "type: " (symbol->string (bi-type bi))))
    format-module-documentation
    (l (module-name md) "any (string ...) -> string" (string-join md "\n"))
    format-modules-documentation (l (mds) "(string ...) -> string" (apply string-append mds)))

  (set! documentation-display-formats
    (pair (pair (q itpn) display-format-itpn) documentation-display-formats)))
