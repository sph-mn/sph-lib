(library (sph lang itml eval shtml)
  (export
    itml-shtml-eval
    itml-shtml-eval-port
    itml-shtml-eval-string
    itml-shtml-lines
    sph-lang-itml-eval-shtml-description)
  (import
    (guile)
    (sph)
    (sph hashtable)
    (sph lang itml eval)
    (sph lang itml read)
    (sph list)
    (sph set)
    (sph web shtml))

  (define sph-lang-itml-eval-shtml-description
    "evaluate inline code expressions and translate itml to shtml")

  (define (descend-handle-association a re-descend sources depth data)
    (let ((keyword (first a)) (re-descend* (l (a) (re-descend a sources depth data))))
      (append (if (string? keyword) (list keyword) (map re-descend* keyword))
        (pair ": " (map re-descend* (tail a))))))

  (define-as ascend-ht ht-create-symbol
    line (l (a . b) (if (null? a) "" a))
    inline-expr itml-eval-asc-inline-expr
    line-expr itml-eval-asc-line-expr indent-expr itml-eval-asc-indent-expr)

  (define-as descend-ht ht-create-symbol
    inline-scm-expr itml-eval-desc-inline-scm-expr
    line-scm-expr itml-eval-desc-line-scm-expr
    association descend-handle-association
    indent-scm-expr itml-eval-desc-indent-scm-expr
    indent-desc-expr itml-eval-desc-indent-expr
    escaped-association-infix (l a ":") double-backslash (l a "\\"))

  (define itml-shtml-lines
    (let*
      ( (inline-html-tags
          (apply set-create-symbol
            (list-q span a object img script select button input label select textarea)))
        (inline-html-tag? (l (a) (ht-ref inline-html-tags a))) (map-line (l (a) (list (q p) a)))
        (map-line-list (l (a) (pair (q p) a))))
      (l (a)
        "list integer -> list
        the top-level is interpreted as a list of lines. this procedure inserts line breaks between string or symbol elements,
        removes empty lists and merges sub-lists that are not tag elements.
        sub-expression creators are supposed to handle line breaks themselves"
        (if (null? a) a
          (let ((a (first a)) (r (itml-shtml-lines (tail a))))
            (if (list? a)
              (if (null? a) r
                (let (prefix (first a))
                  (pair
                    (if (symbol? prefix) (if (inline-html-tag? prefix) (map-line a) a)
                      (map-line-list a))
                    r)))
              (pair (map-line a) r)))))))

  (define itml-shtml-eval
    (let
      (eval
        (itml-eval* descend-ht ascend-ht
          ; terminal
          (l (a . b) (if (eq? (q line-empty) a) (q (br)) a)) #f
          ; ascend-alt
          (let
            (section? (l (a) (and (list? a) (> (length a) 1) (not (eq? (q section) (first a))))))
            (l (a sources depth . b)
              (if (section? a) (shtml-section depth (first a) (itml-shtml-lines (tail a))) a)))))
      (l (a itml-state) "list list -> sxml" (itml-shtml-lines (eval a itml-state)))))

  (define (itml-shtml-eval-port a . b) (apply itml-shtml-eval (port->itml-parsed a) b))
  (define (itml-shtml-eval-string a . b) (apply itml-shtml-eval (string->itml-parsed a) b)))
