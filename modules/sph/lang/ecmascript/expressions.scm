(library (sph lang ecmascript expressions)
  (export
    es-apply
    es-apply-nc
    es-chain
    es-chain-nc
    es-compound-nc
    es-declare
    es-declare-nc
    es-define
    es-define-nc
    es-environment
    es-environment-nc
    es-function
    es-function-nc
    es-function-with-rest-args
    es-identifier
    es-if
    es-if-statement
    es-named-function-nc
    es-object
    es-object-nc
    es-ref
    es-regexp-nc
    es-set!
    es-set-nc!
    es-statement-nc
    es-string
    sph-lang-ecmascript-expressions-description
    es-try-catch-finally-nc
    es-value
    es-vector
    es-vector-nc
    list->es-vector)
  (import
    (guile)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph list)
    (sph string))

  (define sph-lang-ecmascript-expressions-description "create ecmascript syntax strings")

  ;generating ecmascript expressions as strings.
  ;by default, the functions convert input values from scheme datatypes,
  ;the variants with a -nc suffix are non-converting.
  (define* (es-apply-nc proc #:optional (args "")) (string-append proc "(" args ")"))

  (define* (es-apply proc #:optional (args (list)))
    (es-apply-nc (es-identifier proc) (string-join (map es-value args) ",")))

  (define (es-chain-nc proc base args) "string string [string ...] -> string"
    (es-apply-nc (string-append base "." proc) args))

  (define (es-chain proc base . args)
    "any any [any ...] -> string
    chains procedure applications"
    (es-chain-nc (any->string proc) (es-identifier base) (string-join (map es-identifier args) ",")))

  (define (es-declare-nc . names) (string-append "var " (string-join names ",")))
  (define (es-declare . names) (apply es-declare-nc (map es-identifier names)))

  (define* (es-define-nc . names-and-values) "string string -> string"
    (string-append "var "
      (if (= 1 (length names-and-values)) (first names-and-values)
        (string-join (map-slice 2 (l (a b) (string-append a "=" b)) names-and-values) ","))))

  (define* (es-define . names-and-values) "any [any] -> string"
    (string-append "var "
      (if (= 1 (length names-and-values)) (es-identifier (first names-and-values))
        (string-join
          (map-slice 2 (l (a b) (string-append (es-identifier a) "=" (es-value b)))
            names-and-values)
          ","))))

  (define (es-environment-nc a)
    (es-object-nc (map (l (e) (if (pair? e) (pair (first e) (tail e)) (pair e e))) a)))

  (define (es-environment a)
    "(string ...) -> string
    creates an object where key-names are also identifiers for the values"
    (es-object (map (l (e) (if (pair? e) (pair (first e) (tail e)) (pair e e))) a)))

  (define (es-named-function-nc name body identifier-list)
    (string-append "function " name " " identifier-list "{" body "}"))

  (define* (es-function-nc body #:optional (identifier-list "()"))
    (string-append "(function" identifier-list "{" body "})"))

  (define* (es-function #:optional (body "") (formals (list)) #:key rest name)
    (let
      ( (formals (es-identifier-list formals))
        (body
          (if rest
            (string-append (es-rest-args (length formals) (es-identifier rest)) (or body ""))
            (or body ""))))
      (if name (es-named-function-nc (es-identifier name) body formals)
        (es-function-nc body formals))))

  (define (es-identifier a)
    (cond ((symbol? a) (symbol->string a)) ((string? a) a)
      (else (raise (q cannot-convert-to-es-identifier)))))

  (define (es-identifier-list a)
    (parenthesise
      (if (list? a) (string-join (map es-identifier a) ",")
        (if (or (symbol? a) (string? a)) (es-identifier a)
          (raise (q cannot-convert-to-es-identifier))))))

  (define* (es-if test consequent #:optional alternate)
    "string string [string] -> string
    create an if expression"
    (string-append test "?" consequent ":" (if alternate alternate "undefined")))

  (define* (es-if-statement test consequent #:optional alternate)
    "string string [string] -> string
    create an if expression"
    (string-append "if(" test
      "){" consequent "}" (if alternate (string-append "else{" alternate "}") "")))

  (define (es-compound-nc a) (string-append "{" a "}"))

  (define* (es-statement-nc keyword body #:optional a)
    (string-append keyword (if a (parenthesise a) "") (es-compound-nc body)))

  (define (es-object a) "list:alist -> string"
    (string-append "{" (string-join (alist-map single-assoc a) ",") "}"))

  (define (es-object-nc a) (string-append "{" (string-join (alist-map single-assoc-nc a) ",") "}"))
  (define (es-new-nc name args) (string-append "new " (es-apply-nc name)))
  (define (es-ref a key) (string-append (es-identifier a) "[" (es-value key) "]"))

  (define* (es-regexp-nc pattern #:optional (modifiers ""))
    (string-append "/" pattern "/" modifiers))

  (define (es-rest-args formals-count rest-formal)
    "integer string -> string
    can be inserted into a function body to support rest arguments."
    (string-append (es-define-nc rest-formal (string-append "new Array(arguments.length)"))
      ";for(var ___i=" (number->string formals-count)
      ";___i<arguments.length;___i+=1){" rest-formal ".push(" (es-ref (q arguments) (q ___i)) ")" "}"))

  (define (es-set-nc! . name/value)
    (string-join (map-slice 2 (l (name value) (string-append name "=" value)) name/value) ";"))

  (define (es-set! . name/value)
    (string-join
      (map-slice 2 (l (name value) (string-append (es-identifier name) "=" (es-value value)))
        name/value)
      ";"))

  (define-as es-escape-single-char alist
    "\"" "\\\"" "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v")

  (define (es-string str)
    (string-enclose
      (fold (l (e r) (string-replace-string r (first e) (tail e))) str es-escape-single-char) "\""))

  (define* (es-try-catch-finally-nc try #:optional catch-formal catch finally)
    (string-append "try{" try
      "}" (if catch (string-append "catch(" (if catch-formal catch-formal "") "){" catch "}") "")
      (if finally (string-append "finally{" finally "}") "")))

  (define (es-value a) "handles the default conversions between scheme and ecmascript types"
    (cond ((symbol? a) (symbol->string a)) ((string? a) (es-string a))
      ((number? a) (number->string a)) ((vector? a) (vector->es-vector a))
      ((boolean? a) (if a "true" "false"))
      ((list? a) (if (list-alist? a) (es-object a) (list->es-vector a)))
      ((pair? a) (es-vector (first a) (tail a))) ((ht? a) (ht->es-object a))
      ((char? a) (string-enclose (any->string a) "\"")) (else (q cannot-convert-to-es))))

  (define (es-vector-nc contents) (string-append "[" (string-join contents ",") "]"))
  (define (es-vector . contents) (es-vector-nc (map es-value contents)))

  (define (ht->es-object a)
    (string-append "{"
      (string-join
        (ht-fold (l (key value prev) (pair (single-assoc key value) prev)) (list) a) ",")
      "}"))

  (define (list->es-vector a) (apply es-vector a))

  (define (single-assoc key value)
    (string-append (es-value (any->string key)) ":" (es-value value)))

  (define-as es-escape-single-char-2 alist
    "\n" "\\n" "\b" "\\b" "\f" "\\f" "\r" "\\r" "\t" "\\t" "\v" "\\v")

  (define (conditional-es-string a)
    (let
      (r (fold (l (e r) (string-replace-string r (first e) (tail e))) a es-escape-single-char-2))
      (if (eqv? #\" (string-ref r 0)) r (string-enclose r "\""))))

  (define (single-assoc-nc key value)
    (string-append (conditional-es-string (any->string key)) ":" value))

  (define (vector->es-vector a) (list->es-vector (vector->list a))))
