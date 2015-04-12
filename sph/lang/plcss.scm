(library (sph lang plcss)
  (export
    css
    plcss->css
    plcss->css-string)
  (import
    (rnrs base)
    (sph)
    (only (guile) display string-join)
    (only (sph list) map-slice)
    (only (sph string) any->string)
    (only (sph tree) flatten)
    (only (srfi srfi-1) break))

  ;a scheme-datum to css/cascading-stylesheets transcompiler that can be used inline with other scheme code.
  ;the name is an abbreviation of prefixed-list-css
  (define (symbol?->string arg) (if (symbol? arg) (symbol->string arg) arg))
  (define-syntax-rule (at-prefix? arg) (eqv? #\@ (string-ref arg 0)))

  (define-syntax-rule (join-selector arg)
    (apply string-append
      (tail
        (fold
          (l (ele result)
            (if (eqv? #\& (string-ref ele 0)) (pair (substring ele 1) result)
              (pairs " " ele result)))
          (list) arg))))

  (define-syntax-rule (join-properties arg)
    (string-join
      (map-slice 2 (l (a b) (string-append (symbol?->string a) ":" (any->string b))) arg)
      ";" (q suffix)))

  (define (join-rule context properties-string rules)
    (if (null? context) (string-append properties-string (rules->string rules context))
      (string-append (join-selector context) "{"
        properties-string "}" (if rules (rules->string rules context) ""))))

  (define-syntax-rule (rule->string-without-rules prefix context properties-string)
    (if (at-prefix? prefix) (string-append prefix "{" (join-rule context properties-string #f) "}")
      (let (context (pair prefix context)) (join-rule context properties-string #f))))

  (define-syntax-rule (rule->string-with-rules prefix context properties-string rules)
    (if (at-prefix? prefix)
      (string-append prefix "{" (join-rule context properties-string rules) "}")
      (join-rule (pair prefix context) properties-string rules)))

  (define (rule->string arg context) "list/string list -> string"
    (if (string? arg) arg
      (let (prefix (first arg))
        (call-with-values (l () (break list? (tail arg)))
          (l (properties rules)
            (if (list? prefix)
              (if (null? properties)
                (if (null? rules) ""
                  (map (l (prefix) (rules->string rules (pair prefix context))) prefix))
                (let (properties-string (join-properties properties))
                  (map
                    (if (null? rules)
                      (l (prefix) (rule->string-without-rules prefix context properties-string))
                      (l (prefix) (rule->string-with-rules prefix context properties-string rules)))
                    prefix)))
              (if (null? properties)
                (if (null? rules) ""
                  (if (at-prefix? prefix)
                    (string-append prefix "{" (rules->string rules context) "}")
                    (rules->string rules (pair prefix context))))
                (if (null? rules)
                  (rule->string-without-rules prefix context (join-properties properties))
                  (rule->string-with-rules prefix context (join-properties properties) rules)))))))))

  (define (rules->string arg context)
    (apply string-append (flatten (map (l (ele) (rule->string ele context)) arg))))

  (define (plcss->css-string exprs) "(rule ...) -> string" (rules->string exprs (list)))
  (define (plcss->css exprs port) "(rule ...) port ->" (display (plcss->css-string exprs) port))
  (define-syntax-rule (css rules ...) (plcss->css-string (quasiquote (rules ...)))))