(library (sph lang scm-format format)
  (export
    format-application
    format-docstring
    format-hash-bang
    format-lambda
    format-let
    format-library
    format-list
    format-list-assoc
    format-range-comment
    format-scsh-block-comment
    format-semicolon-comment
    format-string
    string-join-with-vertical-spacing)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (sph)
    (sph hashtable)
    (sph list)
    (sph string)
    (sph tree)
    (only (sph one) round-even)
    (only (srfi srfi-1) reverse!))

  (define (add-multiple-leading-parenthesis-spacing config lines) "( (content ..."
    (if
      (and (hashtable-ref config (q multiple-leading-parenthesis-spacing))
        (length-greater-one? lines))
      (map
        (l (e)
          (if (string-contains e "\n")
            (successive-parentheses-indentation e (hashtable-ref config (q indent-string))) e))
        lines)
      lines))

  (define-syntax-rule (create-indent config current-indent)
    (string-multiply (hashtable-ref config (q indent-string)) current-indent))

  (define-syntax-rule (create-vertical-spacing spacing-value)
    (string-multiply "\n" (+ 1 spacing-value)))

  (define-syntax-rule (create-vertical-spacing* spacing)
    (if (string? spacing) spacing (create-vertical-spacing spacing)))

  (define (format-application-expr-proc config current-indent)
    (hashtable-quoted-bind config
      (indent-string max-chars-per-line max-exprs-per-line-start
        max-exprs-per-line-middle max-exprs-per-line-end)
      (let (indent-length (* current-indent (string-length indent-string)))
        (l (rest r line line-expr-length line-expr-count)
          (let* ((e (first rest)) (expr-string (tail e)))
            ;e := (current-line-expr-length . expression-string)
            (if (string-contains expr-string "\n")
              (handle-newline-subexpression rest expr-string line r)
              (if
                (line-full? e indent-length
                  line-expr-count line-expr-length
                  rest max-chars-per-line
                  max-exprs-per-line-end max-exprs-per-line-middle max-exprs-per-line-start r)
                (list (if (null? line) r (pair (string-join (reverse line) " ") r))
                  (list expr-string) (first e) 1)
                (list r (pair expr-string line)
                  (+ line-expr-length (first e)) (+ 1 line-expr-count)))))))))

  (define (format-application-prepare-exprs a)
    (map (l (e) ((l (e-string) (pair (string-length e-string) e-string)) (any->string e))) a))

  (define (format-import a recurse config current-indent)
    (format-list (format-import-spec a recurse config (+ 1 current-indent)) config
      current-indent 1 1 1))

  (define (format-import-map proc a)
    (map (l (a) (if (symbol? a) (symbol->string a) (if (null? a) "()" (proc a)))) a))

  (define (comment? a)
    (and (list? a) (not (null? a))
      (or (eqv? (q semicolon-comment) (first a)) (eqv? (q range-comment) (first a)))))

  (define (format-import-set a recurse config current-indent)
    (case (first a)
      ( (except library only prefix rename)
        (format-list
          (format-import-map (l (a) (format-import-set a recurse config (+ 1 current-indent))) a)
          config current-indent (if (> (length a) 4) 2 (inf)) 1 1))
      (else
        (if (comment? a) (first (recurse a (+ current-indent 1)))
          (format-list a config (+ 1 current-indent) (inf) (inf) (inf))))))

  (define (format-import-spec a recurse config current-indent)
    (format-import-map
      (l (a)
        (if (eqv? (q for) (first a))
          (format-list
            (match a
              ( (for import-set import-level ...)
                (pairs for (format-import-set import-set recurse config current-indent)
                  import-level)))
            config current-indent (inf) (inf) (inf))
          (format-import-set a recurse config current-indent)))
      a))

  (define-syntax-rule (handle-newline-subexpression rest expr-string line r)
    (list
      (pair
        (if (and (not (null? (tail rest))) (string-suffix? "\n" expr-string))
          (string-drop-right expr-string 1) expr-string)
        (if (null? line) r (pair (string-join (reverse line) " ") r)))
      (list) 0 0))

  (define-syntax-rule
    (line-full? e indent-length line-expr-count line-expr-length rest max-chars-per-line
      max-exprs-per-line-end
      max-exprs-per-line-middle
      max-exprs-per-line-start
      r)
    (let (line-length (+ (+ line-expr-length (- line-expr-count 1)) indent-length))
      (and
        (or
          ;(+ line remaining-exprs) does not fit on one line
          (> (+ line-length (apply + (map first rest)) (- (length rest) 1)) max-chars-per-line)
          (>= (length rest) max-exprs-per-line-end))
        (or (= (if (null? r) max-exprs-per-line-start max-exprs-per-line-middle) line-expr-count)
          ;(+ line current-element) does not fit on one line
          (> (+ line-length (first e)) max-chars-per-line)))))

  (define-syntax-rule (map-recurse recurse a current-indent)
    (map (l (e) (first (recurse e current-indent))) a))

  (define (parenthesise-indented-list indent a)
    (string-append "(" a (if (string-suffix? "\n" a) (string-append indent ")") ")")))

  (define (successive-parentheses-indentation a indent-string)
    (let (index (string-skip a #\())
      (if (and index (> index 1))
        (string-append
          (string-join (string-split (substring a 0 (- index 1)) #\()
            (string-append "(" (string-drop indent-string 1)))
          "(" (substring a index))
        a)))

  (define (format-application a config current-indent) "list hashtable integer -> string"
    (let*
      ((indent (create-indent config current-indent)) (line-spacing (string-append "\n" indent)))
      (apply
        (l (r line . rest)
          (parenthesise-indented-list indent
            (string-join
              (add-multiple-leading-parenthesis-spacing config
                ;add the last line
                (reverse! (if (null? line) r (pair (string-join (reverse line) " ") r))))
              line-spacing)))
        (pair-fold-multiple (format-application-expr-proc config current-indent)
          (format-application-prepare-exprs a)
          ;the following 1 is the initial line-length including the beginning parenthesis
          (list) (list) 1 0))))

  (define (format-docstring a config current-indent)
    "parse a string, separate syntax-required indent from custom string indent and add current-indent"
    (let*
      ( (indent-string (hashtable-ref config (q indent-string)))
        (indent (string-multiply indent-string current-indent))
        (lines
          (map
            (l (e)
              (pair
                (let (skip-index (string-skip-string e indent-string))
                  (if skip-index (round-even (/ skip-index (string-length indent-string)))
                    current-indent))
                (string-trim-string e indent-string)))
            (string-split a #\newline)))
        (min-indent (if (null? (tail lines)) 0 (apply min (map first (tail lines)))))
        ;remove syntax-indent
        (lines
          (pair (tail (first lines))
            (map
              (l (e)
                (string-append indent (string-multiply indent-string (- (first e) min-indent))
                  (tail e)))
              (tail lines)))))
      (format-string (string-join lines "\n"))))

  (define (format-hash-bang a recurse config current-indent)
    (list (string-append "#!" (first (tail a)) "\n!#") #f))

  (define (format-lambda a recurse config current-indent)
    (list
      (format-list
        (pair (first a)
          (match (tail a)
            ( (formals body ...)
              (pair
                (if (list? formals)
                  (format-list (map-recurse recurse formals current-indent) config
                    (+ 1 current-indent) (inf) 1 1)
                  formals)
                (if (and (list? body) (not (null? body)))
                  (if (string? (first body))
                    (pair (format-docstring (first body) config current-indent)
                      (map-recurse recurse (tail body) current-indent))
                    (map-recurse recurse body current-indent))
                  body)))
            (_
              ;probably not an effective lambda but data instead
              (tail a))))
        config current-indent
        3 (hashtable-ref config (q max-exprs-per-line-middle))
        (hashtable-ref config (q max-exprs-per-line-end)))
      #f))

  (define (format-let a recurse config current-indent)
    (list
      (format-application (map-recurse recurse a current-indent)
        (match a ((let (? symbol?) _ ...) (hashtable-set-multiple config (q max-exprs-per-line-start) 3))
          (else config))
        current-indent)
      #f))

  (define (format-library a recurse config current-indent)
    (list
      (let
        ( (indent (create-indent config current-indent))
          (vertical-spacing
            (create-vertical-spacing (hashtable-ref config (q toplevel-vertical-spacing)))))
        (match (tail a)
          ( (name exports imports body ...)
            (apply string-append "("
              (symbol->string (first a)) " "
              (format-list name config current-indent (inf) (inf) (inf)) "\n"
              indent (format-import exports recurse config (+ 1 current-indent))
              "\n" indent
              (format-import imports recurse config (+ 1 current-indent))
              (if (null? body) (list ")")
                (list vertical-spacing indent
                  (string-join-with-vertical-spacing
                    (map (l (e) (first (recurse e current-indent))) body) indent
                    vertical-spacing (hashtable-ref config (q toplevel-vertical-spacing-oneline)))
                  ")"))))
          (_ a)))
      #f))

  (define (format-list a config current-indent start middle end)
    (format-application a
      (hashtable-set-multiple config
        ;the line-char-length limitation has always precedence
        (q max-exprs-per-line-start) start
        (q max-exprs-per-line-middle) middle (q max-exprs-per-line-end) end)
      current-indent))

  (define (format-list-assoc a recurse config current-indent)
    (list
      (match a
        ( (let-macro (assoc ...) body ...)
          (format-application
            (pairs (q let-macro)
              (format-application (map-recurse recurse assoc (+ 1 current-indent))
                (let*
                  ((n (hashtable-ref config (q max-exprs-per-line-start))) (n (- n (modulo n 2))))
                  (hashtable-set-multiple config (q max-exprs-per-line-start)
                    n (q max-exprs-per-line-end) n))
                (+ 1 current-indent))
              (map-recurse recurse body current-indent))
            config current-indent))
        (_ a))
      #f))

  (define (format-range-comment a recurse config current-indent)
    (list
      (string-append "#;("
        (string-join (tail a)
          (string-append "\n"
            (string-multiply (hashtable-ref config (q indent-string)) current-indent)))
        ")")
      #f))

  (define (format-semicolon-comment a recurse config current-indent)
    (list (string-append ";" (first (tail a)) "\n") #f))

  (define (format-scsh-block-comment a recurse config current-indent)
    (list (string-append "#!" (first (tail a)) "!#\n") #f))

  (define (format-string a . rest) (string-append "\"" a "\""))

  (define (string-join-with-vertical-spacing a indent vertical-spacing vertical-spacing-oneline)
    (if (null? a) ""
      (if (length-eq-one? a) (first a)
        (string-join
          (let (vertical-spacing-oneline (create-vertical-spacing* vertical-spacing-oneline))
            (map (l (e) (if (string-suffix? "\n" e) (string-drop-right e 1) e))
              ;join one-line expressions
              (flatten
                (map-successive
                  (l (e)
                    ;consider newlines, they designate a required newline for an expression
                    (not (or (string-null? e) (let (a (string-contains e "\n")) (and a (< a (- (string-length e) 1)))))))
                  (l matches
                    (string-join
                      (map (l (e) (if (string-suffix? "\n" e) (string-drop-right e 1) e)) matches)
                      (string-append vertical-spacing-oneline indent)))
                  a))))
          (string-append (create-vertical-spacing* vertical-spacing) indent))))))