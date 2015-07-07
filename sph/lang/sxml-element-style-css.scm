(library (sph lang sxml-element-style-css)
  (export
    sxml-element-style-css)
  (import
    (guile)
    (ice-9 match)
    (rnrs base)
    (sph)
    (sph hashtable)
    (sph lang plcss)
    (sph list)
    (sph string)
    (sph tree)
    (except (rnrs hashtables) hashtable-ref)
    (except (srfi srfi-1) map))

  (define (sxml-element-style-css a class+style)
    "sxml hashtable:(hashtable (string (plcss ...))) -> sxml
    adds element styles for classes in the class attributes or tags.
    classes with styles are removed.
    this can be used to apply styles to html for which external style sheets or <style>-tags are disallowed"
    (define class+style-classes (vector->list (hashtable-keys class+style)))
    (define (get-style a) (match a ((style b ...) b) (_ #f)))
    (define (get-class-names a) "(class _ ...) -> (string ...)"
      (string-split (apply string-append (map symbol?->string (tail a))) #\space))
    (define (create-class-attribute class-names) (list (q class) (string-join class-names " ")))
    (define (extract-class-names attributes) "list -> (class-names attributes)"
      (fold-multiple
        (l (e r-class-names r-attributes)
          (if (list-prefix? e (q class))
            (let (class-names (get-class-names e))
              (let
                ;matching class names are removed
                ( (keep (complement class-names class+style-classes))
                  (translate (intersection class-names class+style-classes)))
                (list (if (null? translate) r-class-names (append translate r-class-names))
                  (if (null? keep) r-attributes (pair (create-class-attribute keep) r-attributes)))))
            (list r-class-names (pair e r-attributes))))
        attributes (list) (list)))
    (define (class-names->style-string a)
      (apply string-append
        (filter-map
          (l (e)
            (let (style-data (hashtable-ref class+style e))
              (if style-data (plcss-element-style->css-string style-data) #f)))
          a)))
    (define (insert-styles class-names attributes)
      (let*
        ( (new-style-string (class-names->style-string class-names))
          (existing-style (find (l (e) (list-prefix? e (q style))) attributes)))
        (if (string-null? new-style-string) attributes
          (if existing-style
            (map-one (l (e) (list-prefix? e (q style))) (l (e) (append e (list new-style-string)))
              attributes)
            (pair (list (q style) new-style-string) attributes)))))
    (define (add-class-styles attributes)
      (apply
        (l (class-names attributes)
          (if (null? class-names) attributes (insert-styles class-names attributes)))
        (extract-class-names attributes)))
    (define (add-tag-styles a)
      (match a
        ( (tag-name ((quote @) attributes ...) body ...)
          (pairs tag-name (pair (q @) (insert-styles (list (symbol->string tag-name)) attributes))
            body))
        ( (tag-name body ...)
          (pair tag-name
            (let (style-string (class-names->style-string (list (symbol->string tag-name))))
              (if (string-null? style-string) body
                (pair (list (q @) (list (q style) style-string)) body)))))))
    (tree-transform a
      (l (e recurse)
        (match e (((quote @) a ...) (list (pair (q @) (add-class-styles a)) #f)) (_ (list #f #t))))
      (l (e) (match e (((? symbol?) _ ...) (add-tag-styles e)) (_ e))) identity)))