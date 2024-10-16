(define-module (sph lang plcss))
(use-modules ((srfi srfi-1) #:select (fold fold-right)))

(export css css-style
  plcss->css plcss->css-string plcss-element-style->css-string sph-lang-plcss-description)

(define sph-lang-plcss-description
  "s-expression language that compiles to css.
   can also be used inline with other scheme code.
   plcss: prefixed-list-css.
   syntax
     css-style :: properties-key/value ...
     for css properties without selectors. use case: inline-styles in xml attributes")

(define (break pred clist)
  "Return two values, the longest initial prefix of LST whose elements
   all fail the predicate PRED, and the remainder of LST."
  (let lp ((clist clist) (rl (quote ())))
    (if (or (null? clist) (pred (car clist))) (values (reverse! rl) clist)
      (lp (cdr clist) (cons (car clist) rl)))))

(define (flatten a)
  "list -> (non-list ...)
   replace sublists with their content, resulting in a list that does not contain lists"
  (fold-right (lambda (e r) (if (list? e) (append (flatten e) r) (cons e r))) (list) a))

(define (map-slice slice-length f a)
  "integer procedure:{any ... -> any} list -> list
   call \"f\" with each \"slice-length\" number of consecutive elements of \"a\""
  (let loop ((rest a) (slice (list)) (slice-ele-length 0) (r (list)))
    (if (null? rest) (reverse (if (null? slice) r (cons (apply f (reverse slice)) r)))
      (if (= slice-length slice-ele-length)
        (loop (cdr rest) (list (car rest)) 1 (cons (apply f (reverse slice)) r))
        (loop (cdr rest) (cons (car rest) slice) (+ 1 slice-ele-length) r)))))

(define (any->string a)
  "any -> string
   generalized string conversion function.
   get the string representation for the value of an object.
   symbols like \".\" are converted to \"#{.}\" using display."
  (if (string? a) a
    (if (symbol? a) (symbol->string a) (call-with-output-string (lambda (port) (display a port))))))

(define (symbol?->string a) (if (symbol? a) (symbol->string a) a))
(define-syntax-rule (at-prefix? a) (eqv? #\@ (string-ref a 0)))

(define-syntax-rule (join-selector a)
  (apply string-append
    (cdr
      (fold
        (lambda (a result)
          (if (eqv? #\& (string-ref a 0)) (cons (substring a 1) result) (cons* " " a result)))
        (list) a))))

(define-syntax-rule (join-properties a) "list -> string"
  (string-join
    (map-slice 2 (lambda (b c) (string-append (symbol?->string b) ":" (any->string c))) a) ";"
    (quote suffix)))

(define (join-rule context properties-string rules) "list string false/list -> string"
  (if (null? context) (string-append properties-string (if rules (rules->string rules context) ""))
    (string-append (join-selector context) "{"
      properties-string "}" (if rules (rules->string rules context) ""))))

(define-syntax-rule (rule->string-without-rules prefix context properties-string)
  "_ list string -> string"
  (if (at-prefix? prefix) (string-append prefix "{" (join-rule context properties-string #f) "}")
    (let ((context (cons prefix context))) (join-rule context properties-string #f))))

(define-syntax-rule (rule->string-with-rules prefix context properties-string rules)
  (if (at-prefix? prefix)
    (string-append prefix "{" (join-rule context properties-string rules) "}")
    (join-rule (cons prefix context) properties-string rules)))

(define (rule->string a context) "list/string list -> string"
  (if (string? a) a
    (let ((prefix (car a)))
      (call-with-values (lambda () (break list? (cdr a)))
        (lambda (properties rules)
          (if (list? prefix)
            (if (null? properties)
              (if (null? rules) ""
                (map (lambda (prefix) (rules->string rules (cons prefix context))) prefix))
              (let ((properties-string (join-properties properties)))
                (map
                  (if (null? rules)
                    (lambda (prefix) (rule->string-without-rules prefix context properties-string))
                    (lambda (prefix)
                      (rule->string-with-rules prefix context properties-string rules)))
                  prefix)))
            (if (null? properties)
              (if (null? rules) ""
                (if (at-prefix? prefix)
                  (string-append prefix "{" (rules->string rules context) "}")
                  (rules->string rules (cons prefix context))))
              (if (null? rules)
                (rule->string-without-rules prefix context (join-properties properties))
                (rule->string-with-rules prefix context (join-properties properties) rules)))))))))

(define (rules->string a context)
  (apply string-append (flatten (map (lambda (a) (rule->string a context)) a))))

(define (plcss->css-string exprs) "(rule ...) -> string" (rules->string exprs (list)))
(define (plcss->css exprs port) "(rule ...) port ->" (display (plcss->css-string exprs) port))
(define-syntax-rule (css rules ...) (plcss->css-string (quasiquote (rules ...))))
(define (plcss-element-style->css-string a) (join-properties a))
(define-syntax-rule (css-style properties ...) (join-properties (quasiquote (properties ...))))
