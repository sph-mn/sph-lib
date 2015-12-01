;the assign syntax is for applying css styles inline, while css inclusion in the head of the html is handled automatically

(library (sph lang docl env sxml-html-style-assign)
  (export
    a
    assign
    assign-stylesheet
    bttl-class-prefix)
  (import
    (guile)
    (sph))

  (define-syntax-rule (a a ...)
    ;alias for assign
    (assign a ...))

  (define temporary-class
    (let (count 0)
      (lambda ()
        "create a temporary style class. the classes are numbered,
        each new one counted upwards as long as the process runs."
        (if (> count 9223372036854775806) (set! count 0) (set! count (+ 1 count)))
        (string-append bttl-class-prefix (number->string count 32)))))

  (define bttl-class-prefix "bttl-")

  (define (style-value a)
    "string\\number\\symbol -> string
    handle values for a style property"
    (if (string? a) a
      (if (number? a) (number->string a)
        (if (symbol? a) (symbol->string a) (throw (q wrong-type-for-argument) a)))))

  (define (style-property a)
    "pair -> string
    handle one property to value expression"
    (let
      ( (property-name
          (let ((property-name (first a)))
            (if (string? property-name) property-name (symbol->string property-name))))
        (value (tail a)))
      (string-append property-name ":"
        (if (list? value) (string-join (map style-value value) " ") (style-value value)))))

  (define (assign-stylesheet class styles)
    "list -> string
    create a stylesheet string from the result of handle-assign"
    (string-append "." class "{" (string-join (map style-property styles) ";") "}"))

  (define (primitive-assign class\style text)
    "list string -> text style
    procedure for the assign syntax"
    (call-with-values (l () (partition! symbol? class\style))
      (l (class style)
        (apply
          (l (class . style)
            (list
              (qq
                (span (@ (class (unquote (string-join class " "))))
                  (unquote (text->sxml (any->string text)))))
              style))
          (let (class (map! symbol->string class))
            (if (null? style) (list class)
              (let (temp-class (temporary-class))
                (list (pair temp-class class) (assign-stylesheet temp-class style)))))))))

  (define (assign-stylesheet class styles)
    "list -> string
    create a stylesheet string from the result of handle-assign"
    (string-append "." class "{" (string-join (map style-property styles) ";") "}"))

  (define (primitive-assign class\style text)
    "list string -> text style
    procedure for the assign syntax"
    (call-with-values (l () (partition! symbol? class\style))
      (l (class style)
        (apply
          (l (class . style)
            (list
              (qq
                (span (@ (class (unquote (string-join class " "))))
                  (unquote (text->sxml (any->string text)))))
              style))
          (let (class (map! symbol->string class))
            (if (null? style) (list class)
              (let (temp-class (temporary-class))
                (list (pair temp-class class) (assign-stylesheet temp-class style)))))))))

  (define-syntax-rule (assign class\style ... text)
    ;assign classes and css styles to the contained text
    (primitive-assign (quote (class\style ...)) text)))
