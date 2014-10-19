(library (sph guile-dbi lib)
  (export
    dbi-last-query-ok?
    dbi-query
    guile-dbi-query)
  (import
    (ice-9 streams)
    (rnrs base)
    (sph)
    (only (dbi dbi) dbi-get_row dbi-get_status)
    (only (guile) simple-format)
    (only (sph alist) alist-ref alist-select)
    (rename (dbi dbi) (dbi-query guile-dbi-query)))

  ;dbi abstraction
  ;features
  ;result-stream-creation
  ;-- utility --;

  (define* (make-result-stream selection retrieve) "make a stream of dbi-rows"
    (make-stream
      (if (boolean? retrieve)
        (l (state)
          (if state (let ((value (dbi-get_row state))) (if value (cons value state) value))
            state))
        (if (list? retrieve)
          (l (state)
            (if state
              (let ((value (dbi-get_row state)))
                (if value (cons (alist-select value retrieve) state) value))
              state))
          (l (state)
            ;string
            (if state
              (let ((value (dbi-get_row state)))
                (if value (cons (alist-ref value retrieve) state) value))
              state))))
      selection))

  ;-- exported --;

  (define-syntax-rule (dbi-last-query-ok? connection)
    (eqv? (first (dbi-get_status connection)) 0))

  (define* (dbi-query connection query #:optional retrieve)
    "execute sql-query and result in an (ice-9 stream) of dbi-rows if retrieve is given"
    ;(simple-format #t "~A\n" query)
    (guile-dbi-query connection query)
    (if (dbi-last-query-ok? connection)
      (if retrieve (make-result-stream connection retrieve) connection) #f)))