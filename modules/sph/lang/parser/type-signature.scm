(library (sph lang parser type-signature)
  (export
    parsed-type-signature->string
    string->parsed-type-signature
    (rename (sig peg-type-signature)))
  (import
    (guile)
    (ice-9 peg)
    (rnrs base)
    (sph)
    (sph tree)
    (only (srfi srfi-1) second))

  (define-peg-pattern ignored-space none " ")
  (define-peg-pattern ignored-ellipsis none "...")
  (define-peg-pattern ignored-bracket-opening none "[")
  (define-peg-pattern ignored-bracket-closing none "]")
  (define-peg-pattern ignored-brace-opening none "{")
  (define-peg-pattern ignored-brace-closing none "}")
  (define-peg-pattern ignored-newline none "\n")
  (define-peg-pattern ignored-arrow none "->")
  (define-peg-pattern ignored-colon none ":")
  (define-peg-pattern ignored-double-colon none "::")
  (define-peg-pattern slash body "/")

  (define-peg-pattern parenthetical body
    (and (? "#") "(" (* (or parenthetical (and (not-followed-by ")") peg-any))) ")"))

  (define-peg-pattern common-delimiter none
    (or ignored-space ignored-newline
      ignored-brace-closing ignored-bracket-closing slash ignored-arrow ignored-colon))

  (define-peg-pattern argument-identifier body
    (+ (or parenthetical (and (not-followed-by common-delimiter) peg-any))))

  (define-peg-pattern argument-element body (or sig-procedure argument-identifier))
  (define-peg-pattern sig-procedure all (and ignored-brace-opening sig ignored-brace-closing))
  (define-peg-pattern alternatives all (and argument-element (* (and slash argument-element))))
  (define-peg-pattern association all (and alternatives (+ (and ignored-colon alternatives))))

  (define-peg-pattern argument body
    (+ (and (not-followed-by common-delimiter) (or association alternatives))))

  (define-peg-pattern repetition-argument all (and argument ignored-space ignored-ellipsis))
  (define-peg-pattern any-argument body (or repetition-argument argument))

  (define-peg-pattern optional-arguments all
    (and ignored-bracket-opening any-argument
      (* (and ignored-space any-argument)) ignored-bracket-closing))

  (define-peg-pattern arguments all
    (and (or optional-arguments any-argument)
      (* (and ignored-space (or optional-arguments any-argument)))))

  (define-peg-pattern sig-line-part body
    (+ (and (not-followed-by (and ignored-space ignored-arrow)) arguments)))

  (define-peg-pattern sig-line all
    (and (? (and sig-line-part ignored-space))
      (+ (and ignored-arrow (? (and ignored-space sig-line-part))))))

  (define-peg-pattern sig-multiline-part all
    (+ (and (not-followed-by "\n\n") (or arguments ignored-newline ignored-space))))

  (define-peg-pattern sig-multiline all
    (and ignored-double-colon (or ignored-newline ignored-space)
      sig-multiline-part ignored-arrow sig-multiline-part))

  (define-peg-pattern sig all
    (or sig-multiline (and sig-line (* (and ignored-newline (* ignored-space) sig-line)))))

  (define (string->parsed-type-signature arg) "string -> list/boolean"
    (peg:tree (match-pattern sig arg)))

  (define* (parsed-type-signature->string arg #:optional line-prefix)
    (let (line-delimiter (if line-prefix (string-append "\n" line-prefix) "\n"))
      (first
        (tree-map-lists
          (l (ele)
            (case (first ele) ((alternatives) (first (tail ele)))
              ((arguments) (string-join (flatten (tail ele)) " "))
              ((sig-line) (string-join (tail ele) " -> "))
              ((sig-multiline) (string-append "::\n" (string-join (tail ele) "\n->\n")))
              ((sig-multiline-part) (string-join (tail ele) "\n"))
              ((sig-procedure) (string-append "{" (second ele) "}"))
              ((sig) (string-join (flatten (tail ele)) line-delimiter))
              ((repetition-argument) (string-append (second ele) " ..."))
              ((association) (string-join (flatten (tail ele)) ":"))
              ((optional-arguments) (string-append "[" (string-join (flatten (tail ele)) " ") "]")) (else ele)))
          (list arg))))))