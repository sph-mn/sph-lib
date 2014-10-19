(library (sph lang ecmascript expressions)
  (export
    es-apply
    es-apply-nc
    es-chain
    es-chain-nc
    es-compound-nc
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
    es-try-catch-finally-nc
    es-value
    es-vector
    es-vector-nc
    list->es-vector)
  (import
    (guile)
    (rnrs base)
    (sph)
    (sph alist)
    (sph hashtable)
    (sph list)
    (sph string)
    (only (rnrs hashtables) hashtable?))

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

  (define* (es-define-nc name #:optional value) "string string -> string"
    (string-append "var " name (if value (string-append "=" value) "")))

  (define* (es-define name #:optional value) "any [any] -> string"
    (es-define-nc (es-identifier name) (if value (es-value value) value)))

  (define (es-environment-nc arg)
    (es-object-nc (map (l (e) (if (pair? e) (pair (first e) (tail e)) (pair e e))) arg)))

  (define (es-environment arg)
    "(string ...) -> string
    creates an object where key-names are also identifiers for the values"
    (es-object (map (l (e) (if (pair? e) (pair (first e) (tail e)) (pair e e))) arg)))

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

  (define (es-identifier arg)
    (cond ((symbol? arg) (symbol->string arg)) ((string? arg) arg)
      (else (raise (q cannot-convert-to-es-identifier)))))

  (define (es-identifier-list arg)
    (parenthesise
      (if (list? arg) (string-join (map es-identifier arg) ",")
        (if (or (symbol? arg) (string? arg)) (es-identifier arg)
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

  (define (es-compound-nc arg) (string-append "{" arg "}"))

  (define* (es-statement-nc keyword body #:optional arg)
    (string-append keyword (if arg (parenthesise arg) "") (es-compound-nc body)))

  (define (es-object arg) "list:alist -> string"
    (string-append "{" (string-join (alist-map single-assoc arg) ",") "}"))

  (define (es-object-nc arg)
    (string-append "{" (string-join (alist-map single-assoc-nc arg) ",") "}"))

  (define (es-new-nc name args) (string-append "new " (es-apply-nc name)))
  (define (es-ref arg key) (string-append (es-identifier arg) "[" (es-value key) "]"))

  (define* (es-regexp-nc pattern #:optional (modifiers ""))
    (string-append "/" pattern "/" modifiers))

  (define (es-rest-args formals-count rest-formal)
    "integer string -> string
    can be inserted into a function body to support rest arguments."
    (string-append (es-define-nc rest-formal (string-append "new Array(arguments.length)"))
      ";for(var ___i=" (number->string formals-count)
      ";___i<arguments.length;___i+=1){" rest-formal ".push(" (es-ref (q arguments) (q ___i)) ")" "}"))

  (define (es-set-nc! . name/value)
    (string-join (reverse (map-slice 2 (l (name value) (string-append name "=" value)) name/value))
      ";"))

  (define (es-set! . name/value)
    (string-join
      (reverse
        (map-slice 2 (l (name value) (string-append (es-identifier name) "=" (es-value value)))
          name/value))
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

  (define (es-value arg) "handles the default conversions between scheme and ecmascript types"
    (cond ((symbol? arg) (symbol->string arg)) ((string? arg) (es-string arg))
      ((number? arg) (number->string arg)) ((vector? arg) (vector->es-vector arg))
      ((boolean? arg) (if arg "true" "false"))
      ((list? arg) (if (list-alist? arg) (es-object arg) (list->es-vector arg)))
      ((pair? arg) (es-vector (first arg) (tail arg))) ((hashtable? arg) (hashtable->es-object arg))
      ((char? arg) (string-enclose (any->string arg) "\"")) (else (q cannot-convert-to-es))))

  (define (es-vector-nc contents) (string-append "[" (string-join contents ",") "]"))
  (define (es-vector . contents) (es-vector-nc (map es-value contents)))

  (define (hashtable->es-object arg)
    (string-append "{"
      (string-join
        (hashtable-fold (l (key value prev) (pair (single-assoc key value) prev)) (list) arg) ",")
      "}"))

  (define (list->es-vector arg) (apply es-vector arg))

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

  (define (vector->es-vector arg) (list->es-vector (vector->list arg))))