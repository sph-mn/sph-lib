(library (sph lang docl env base)
  (export
    a
    assign
    bttl-class-prefix
    scm
    scm*
    sxml)
  (import
    (ice-9 receive)
    (rnrs base)
    (sph)
    (only (guile)
      cons*
      string-split
      string-join)
    (only (sph list)
      length-eq-one?
      list-prefix?
      map-successive)
    (only (sph string) any->string string-multiply)
    (only (sph web html-sxml)
      html-sxml-link
      html-sxml-list->list
      html-sxml-list->table)
    (only (srfi srfi-1)
      append!
      delete-duplicates!
      fold-right
      map!
      partition!
      reverse!))

  (define temporary-class
    (let (count 0)
      (lambda ()
        "create a temporary style class. the classes are numbered,
        	each new one counted upwards as long as the process runs."
        (if (> count 9223372036854775806) (set! count 0) (set! count (+ 1 count)))
        (string-append bttl-class-prefix (number->string count 32)))))

  (define bttl-class-prefix "bttl-")

  (define (text->sxml arg)
    "string -> sxml
    convert newlines in string to (br) and result in an sxml expression"
    (let (arg (string-split arg #\newline))
      (if (length-eq-one? arg) (first arg)
        (tail
          (fold-right (l (ele prev) (cons (q (br)) (cons ele prev))) (list (first arg))
            (tail arg))))))

  (define (style-value arg)
    "string\\number\\symbol -> string
    handle values for a style property"
    (if (string? arg) arg
      (if (number? arg) (number->string arg)
        (if (symbol? arg) (symbol->string arg) (throw (q wrong-type-for-argument) arg)))))

  (define (style-property arg)
    "pair -> string
    handle one property to value expression"
    (let
      ( (property-name
          (let ((property-name (first arg)))
            (if (string? property-name) property-name (symbol->string property-name))))
        (value (tail arg)))
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
                (list (cons temp-class class) (assign-stylesheet temp-class style)))))))))

  (define-syntax-rule (assign class\style ... text)
    ;assign classes and css styles to the contained text
    (primitive-assign (quote (class\style ...)) text))

  (define-syntax-rule (a arg ...)
    ;alias for assign
    (assign arg ...))

  (define (scm arg)
    "s-expr -> any
    evaluates a scheme expression and inserts the result"
    (list arg))

  (define (scm* arg)
    "s-expr -> sxml
    evaluates a scheme expression and formats the result for use as html
    vector -> table
    list -> unordered list
    string -> text"
    (list
      (if (string? arg) (text->sxml arg)
        (if (list? arg) (html-sxml-list->list arg) (any->string arg)))))

  (define-syntax-rule (sxml expr)
    ;return the enclosed expression as sxml.
    ;the sxml is quasiquoted and so can contain dynamically computed expressions
    (list (qq expr))))