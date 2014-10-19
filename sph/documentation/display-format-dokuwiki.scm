(library (sph documentation display-format-dokuwiki)
  (export)
  (import
    (rnrs base)
    (sph)
    (sph alist)
    (sph documentation))

  (define display-format-dokuwiki
    (list (pair (q format-arguments) default-format-arguments)
      (pair (q format-binding-info)
        (l (bi formatted-arguments)
          (let
            ( (format-documentation
                (l (str)
                  (string-join
                    (map (l (str) (string-trim str #\space)) (string-split str #\newline)) "\n"))))
            (string-append "=====" (symbol->string (doc-name bi))
              "=====" "\n"
              (if (string-null? formatted-arguments) ""
                (string-append "arguments - " formatted-arguments "\\\\\n"))
              "type - " (symbol->string (doc-type bi))
              (if (doc-documentation bi)
                (string-append "\\\\\n\\\\\n\\\\\n" (format-documentation (doc-documentation bi))) "")))))
      (pair (q format-module-documentation) (l (module-name md) (string-join md "\n\n")))
      (pair (q format-modules-documentation) (l (mds) (apply string-append mds)))))

  (alist-set! documentation-display-formats (q dokuwiki) display-format-dokuwiki))