(library (sph lang itml eval shtml)
  (export
    itml-shtml-eval
    itml-shtml-eval-file
    itml-shtml-eval-port
    itml-shtml-eval-string
    itml-shtml-false
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

  (define itml-shtml-false "_")

  (define (descend-handle-association a re-descend sources depth data)
    (let ((keyword (first a)) (re-descend* (l (a) (re-descend a sources depth data))))
      (list
        (append (if (string? keyword) (list keyword) (map re-descend* keyword))
          (pair ": " (map re-descend* (tail a)))))))

  (define-as ascend-ht ht-create-symbol-q
    line (l (a . b) (if (null? a) a (list (q p) a)))
    inline-expr itml-eval-asc-inline-expr
    line-expr itml-eval-asc-line-expr indent-expr itml-eval-asc-indent-expr)

  (define (string-if-false proc)
    "when an itml expression evaluates to false, return a string instead, to
     mark the place of a failed expression in the output text. nested scm expressions are not affected"
    (l a (or (apply proc a) itml-shtml-false)))

  (define-as descend-ht ht-create-symbol-q
    inline-scm-expr (string-if-false itml-eval-desc-inline-scm-expr)
    line-scm-expr (string-if-false itml-eval-desc-line-scm-expr)
    indent-scm-expr (string-if-false itml-eval-desc-indent-scm-expr)
    indent-descend-expr (string-if-false itml-eval-desc-indent-expr)
    association descend-handle-association
    escaped-association-infix (l a ":") double-backslash (l a "\\"))

  (define itml-shtml-lines
    (let*
      ( (inline-html-tags
          (apply set-create-symbol
            (list-q span a object img script select button input label select textarea)))
        (inline-html-tag? (l (a) (ht-ref inline-html-tags a))) (line-wrap (l (a) (list (q p) a)))
        (line-list (l (a) (pair (q p) a)))
        (splice-non-tag-lists (l (a) (splice (l (a) (or (null? a) (not (symbol? (first a))))) a))))
      (l (a)
        "list integer -> list
        receives a list of expressions that eventually become separate lines.
        rules:
        * html inline elements are wrapped with <p>
        * html block elements are left as is
        * list contents become <p> contents
        * other elements are wrapped with <p>
        * on the first level of the given list, lists that
          do not correspond to html tags are spliced.
          these lists are assumed to contain result elements
          from dynamic code evaluation"
        (let loop ((a (splice-non-tag-lists a)))
          (if (null? a) a
            (let ((a (first a)) (b (loop (tail a))))
              (if (list? a)
                (let (a (splice-non-tag-lists a))
                  (if (null? a) b
                    (let (prefix (first a))
                      (pair
                        (if (symbol? prefix) (if (inline-html-tag? prefix) (line-wrap a) a)
                          (line-list a))
                        b))))
                (pair (line-wrap a) b))))))))

  (define itml-shtml-eval
    (let
      (eval
        (itml-eval* descend-ht ascend-ht
          ; terminal
          (l (a . b) (if (eq? (q line-empty) a) (q (br)) a)) #f
          ; ascend-alt
          (let (section? (l (a) (and (< 1 (length a)) (not (eq? (q section) (first a))))))
            (l (a sources depth . b)
              "list any integer any ... -> sxml
              on ascend, lists that are not <sections> become sections, with the list prefix being the header,
              and the tail being the section content"
              (if (section? a) (shtml-section depth (first a) (itml-shtml-lines (tail a))) a)))))
      (l (a itml-state) "list list -> sxml" (itml-shtml-lines (eval a itml-state)))))

  (define (itml-shtml-eval-port a . b) (apply itml-shtml-eval (port->itml-parsed a) b))
  (define (itml-shtml-eval-string a . b) (apply itml-shtml-eval (string->itml-parsed a) b))

  (define (itml-shtml-eval-file a . b)
    (call-with-input-file a (l (a) (apply itml-shtml-eval-port a b)))))
