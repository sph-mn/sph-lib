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

  (define* (make-result-stream selection retrieve) "dbi-connection boolean/(string ...)/string -> ice-9-stream
    create a stream object of dbi-rows"
    (make-stream
      (if (boolean? retrieve)
        (l (state)
          (if state (let ((value (dbi-get_row state))) (if value (pair value state) value))
            state))
        (if (list? retrieve)
          (l (state)
            (if state
              (let ((value (dbi-get_row state)))
                (if value (pair (alist-select value retrieve) state) value))
              state))
          (l (state)
            ;string
            (if state
              (let ((value (dbi-get_row state)))
                (if value (pair (alist-ref value retrieve) state) value))
              state))))
      selection))

  (define (dbi-last-query-ok? connection)
    "dbi-connection -> boolean
    true if the last executed query ran successful"
    (eqv? (first (dbi-get_status connection)) 0))

  (define* (dbi-query connection query #:optional retrieve)
    "dbi-connection string [boolean/(string ...)/string] -> ice-9-stream/dbi-connection/false
    execute sql-query and result in an (ice-9 stream) of dbi-rows if retrieve is given"
    (guile-dbi-query connection query)
    (if (dbi-last-query-ok? connection)
      (if retrieve (make-result-stream connection retrieve) connection) #f)))