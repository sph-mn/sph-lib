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
    format-quasiquote
    format-quasisyntax
    format-quote
    format-range-comment
    format-scsh-block-comment
    format-semicolon-comment
    format-string
    format-syntax
    format-test-module
    format-unquote
    format-unsyntax
    format-vector
    string-join-with-vertical-spacing)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (sph)
    (sph hashtable)
    (sph lang scm-format base)
    (sph list)
    (sph string)
    (sph tree)
    (only (srfi srfi-1) last))

  ;formatters for expressions

  (define (round-even a)
    "number -> integer
     floor to the nearest even integer"
    (let (a (inexact->exact (round a))) (+ a (modulo a 2))))

  (define (add-multiple-leading-parenthesis-spacing config lines) "( (content ..."
    (if
      (and (hashtable-ref config (q multiple-leading-parenthesis-spacing))
        (length-greater-one? lines))
      (map
        (l (e)
          (if (string-contains e "\n")
            (consecutive-parentheses-indentation e (hashtable-ref config (q indent-string))) e))
        lines)
      lines))

  (define-syntax-rule (create-indent config current-indent)
    (string-multiply (hashtable-ref config (q indent-string)) current-indent))

  (define-syntax-rule (create-vertical-spacing spacing-value)
    (string-multiply "\n" (+ 1 spacing-value)))

  (define-syntax-rule (create-vertical-spacing* spacing)
    (if (string? spacing) spacing (create-vertical-spacing spacing)))

  (define (format-application-expr-proc config current-indent) "hashtable integer -> list"
    (hashtable-bind config
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

  (define (consecutive-parentheses-indentation a indent-string)
    "string string -> string
     offsets leading parentheses on one line by the level of idendation. example: ( ("
    (let (index (string-skip a #\())
      (if (and index (> index 1))
        (string-append
          (string-join (string-split (substring a 0 (- index 1)) #\()
            (string-append "(" (string-drop indent-string 1)))
          "(" (substring a index))
        a)))

  (define (format-application a config current-indent)
    "list hashtable integer -> string
     format the standard list application form. example (append a b)"
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
    "string hashtable integer -> string
     parses a string and removes outside string indent from using newlines in a continuous string that is indented.
     adds current indent to all lines except the first.
     since old-indent is not available here, old-indent is guessed from the second line.
     if the second line is indented relative to the first line, this indent will unfortunately be removed for all lines"
    (let*
      ( (a (string-trim-both a)) (indent-string (hashtable-ref config (q indent-string)))
        (indent (string-multiply indent-string current-indent))
        (second-line-index (string-index a #\newline)))
      (format-string
        (if second-line-index
          (let*
            ( (indent-to-remove
                (substring a (+ 1 second-line-index)
                  (+ second-line-index
                    (string-skip-string (substring a (+ 1 second-line-index)) indent-string)))))
            (if (hashtable-ref config (q docstring-offset-doublequote))
              (let (lines (string-split a #\newline))
                ; add one extra space to offset the initial doublequote
                (string-append (first lines) "\n"
                  (string-join
                    (map
                      (l (a)
                        (let*
                          ( (a (string-replace-string a indent-to-remove ""))
                            (space-count (string-skip a #\space)))
                          (if (even? space-count) (string-append indent " " a)
                            (string-append indent a))))
                      (tail lines))
                    "\n")))
              (string-replace-string a (string-append "\n" indent-to-remove)
                (string-append "\n" indent))))
          a))))

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
              ;probably not an evaluated lambda but data
              (tail a))))
        config current-indent
        3 (hashtable-ref config (q max-exprs-per-line-middle))
        (hashtable-ref config (q max-exprs-per-line-end)))
      #f))

  (define (format-let a recurse config current-indent)
    (list
      (format-application (map-recurse recurse a current-indent)
        (match a
          ((let (? symbol?) _ ...) (hashtable-set-multiple config (q max-exprs-per-line-start) 3))
          (else config))
        current-indent)
      #f))

  (define (format-test-module a recurse config current-indent)
    "list procedure hashtable integer -> string"
    (list
      (if (= 1 current-indent)
        (let
          ( (indent (create-indent config current-indent))
            (vertical-spacing
              (create-vertical-spacing (hashtable-ref config (q toplevel-vertical-spacing)))))
          (match (tail a)
            ( (name imports body ...)
              (apply string-append "("
                (symbol->string (first a)) " "
                (format-list name config current-indent (inf) (inf) (inf)) "\n"
                indent (format-import imports recurse config (+ 1 current-indent))
                (if (null? body) (list ")")
                  (list vertical-spacing indent
                    (string-join-with-vertical-spacing
                      (map (l (e) (first (recurse e current-indent))) body) indent
                      vertical-spacing (hashtable-ref config (q toplevel-vertical-spacing-oneline)))
                    ")"))))
            (_ a)))
        a)
      #f))

  (define (format-library a recurse config current-indent)
    "list procedure hashtable integer -> string"
    (list
      (if (= 1 current-indent)
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
        a)
      #f))

  (define (format-read-syntax-quote prefix a recurse config current-indent)
    (list
      (if (hashtable-ref config (q use-read-syntax-quote))
        (string-append prefix (first (recurse (second a) current-indent)))
        (format-application (map-recurse recurse a current-indent) config current-indent))
      #f))

  (define (format-quote . a) (apply format-read-syntax-quote "'" a))
  (define (format-quasiquote . a) (apply format-read-syntax-quote "`" a))
  (define (format-unquote . a) (apply format-read-syntax-quote "," a))
  (define (format-syntax . a) (apply format-read-syntax-quote "#'" a))
  (define (format-quasisyntax . a) (apply format-read-syntax-quote "#`" a))
  (define (format-unsyntax . a) (apply format-read-syntax-quote "#," a))

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

  (define (multiline-expression? a) "string -> boolean"
    (let (index-newline (string-contains a "\n"))
      (and index-newline (< index-newline (- (string-length a) 1)))))

  (define (string-remove-trailing-newline a) (if (string-suffix? "\n" a) (string-drop-right a 1) a))

  (define string-join-with-vertical-spacing
    (let
      ( (join-oneline
          (l (a indent vertical-spacing-oneline)
            (let*
              ( (vertical-spacing
                  (string-append (create-vertical-spacing* vertical-spacing-oneline) indent))
                (leading
                  (map-consecutive (l (e) (not (or (string-null? e) (multiline-expression? e))))
                    (l matches
                      (interleave (map string-remove-trailing-newline matches) vertical-spacing))
                    a))
                (index-last (- (length leading) 1)))
              (map-with-index
                (l (index e)
                  (if (= index-last index)
                    (if (list? e)
                      (apply string-append
                        (if (string-prefix? ";" (last e)) (append e (list "\n")) e))
                      (if (string-prefix? ";" e) (string-append e "\n") e))
                    (if (list? e) (apply string-append e) e)))
                leading))))
        (join-multiline
          (l (a indent vertical-spacing)
            ;expressions like comments that modify the rest of the line need a newline at the end or they may affect following parentheses
            (let (index-last (- (length a) 1))
              (string-join
                (map-with-index
                  (l (index e) (if (= index index-last) e (string-remove-trailing-newline e))) a)
                (string-append (create-vertical-spacing* vertical-spacing) indent))))))
      (l (a indent vertical-spacing vertical-spacing-oneline)
        "(string ...) string string string -> string
        join expressions eventually with empty lines inbetween them"
        (if (null? a) ""
          (if (length-eq-one? a) (first a)
            (join-multiline (join-oneline a indent vertical-spacing-oneline) indent
              vertical-spacing)))))))
