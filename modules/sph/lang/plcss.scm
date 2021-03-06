(define-module (sph lang plcss))

(use-modules (srfi srfi-1) (sph) ((sph list) #:select (flatten map-slice))
  ((sph string) #:select (any->string)) ((srfi srfi-1) #:select (break)))

(export css css-style
  plcss->css plcss->css-string plcss-element-style->css-string sph-lang-plcss-description)

(define sph-lang-plcss-description
  "s-expression language that compiles to css.
   can also be used inline with other scheme code.
   plcss: prefixed-list-css.
   syntax
     css-style :: properties-key/value ...
     for css properties without selectors. use case: inline-styles in xml attributes")

(define (symbol?->string a) (if (symbol? a) (symbol->string a) a))
(define-syntax-rule (at-prefix? a) (eqv? #\@ (string-ref a 0)))

(define-syntax-rule (join-selector a)
  (apply string-append
    (tail
      (fold
        (l (ele result)
          (if (eqv? #\& (string-ref ele 0)) (pair (substring ele 1) result) (pairs " " ele result)))
        (list) a))))

(define-syntax-rule (join-properties a) "list -> string"
  (string-join (map-slice 2 (l (b c) (string-append (symbol?->string b) ":" (any->string c))) a)
    ";" (q suffix)))

(define (join-rule context properties-string rules) "list string false/list -> string"
  (if (null? context) (string-append properties-string (if rules (rules->string rules context) ""))
    (string-append (join-selector context) "{"
      properties-string "}" (if rules (rules->string rules context) ""))))

(define-syntax-rule (rule->string-without-rules prefix context properties-string)
  "_ list string -> string"
  (if (at-prefix? prefix) (string-append prefix "{" (join-rule context properties-string #f) "}")
    (let (context (pair prefix context)) (join-rule context properties-string #f))))

(define-syntax-rule (rule->string-with-rules prefix context properties-string rules)
  (if (at-prefix? prefix)
    (string-append prefix "{" (join-rule context properties-string rules) "}")
    (join-rule (pair prefix context) properties-string rules)))

(define (rule->string a context) "list/string list -> string"
  (if (string? a) a
    (let (prefix (first a))
      (call-with-values (l () (break list? (tail a)))
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

(define (rules->string a context)
  (apply string-append (flatten (map (l (ele) (rule->string ele context)) a))))

(define (plcss->css-string exprs) "(rule ...) -> string" (rules->string exprs (list)))
(define (plcss->css exprs port) "(rule ...) port ->" (display (plcss->css-string exprs) port))
(define-syntax-rule (css rules ...) (plcss->css-string (quasiquote (rules ...))))
(define (plcss-element-style->css-string a) (join-properties a))
(define-syntax-rule (css-style properties ...) (join-properties (quasiquote (properties ...))))
