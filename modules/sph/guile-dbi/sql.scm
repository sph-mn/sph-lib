(library (sph guile-dbi sql)
  (export
    dbi-sql-count
    dbi-sql-last-insert-rowid
    dbi-sql-max
    dbi-sql-read
    dbi-sqlite-existing-tables?)
  (import
    (ice-9 streams)
    (rnrs base)
    (sph)
    (sph guile-dbi lib)
    (sph sql)
    (except (dbi dbi) dbi-query)
    (only (sph alist) alist-ref))

  (define (dbi-sql-last-insert-rowid connection)
    "-> integer
    get the row id of the last inserted data element.
    supported by at least sqlite and mysql"
    (tail (first (dbi-get_row (dbi-query connection "select last_insert_rowid()")))))

  (define (dbi-sql-max connection table-name column-name)
    "dbi-connection string string -> integer
    get the maximum value in a column"
    (let*
      ( (max-column (string-append "max(" column-name ")"))
        (max-value
          (alist-ref
            (first
              (stream->list
                (dbi-query connection (string-append "select " max-column " from " table-name))))
            max-column)))
      (if max-value max-value 0)))

  (define dbi-sqlite-existing-tables?
    (let
      ( (create-query
          (l (name)
            (string-append "select count(type) from sqlite_master where type='table' and name='"
              name "' limit 1"))))
      (l (connection . table-names) "
        dbi-connection string ... -> boolean
        test if all tables exists in database"
        (every
          (l (name) (dbi-query connection (create-query name))
            (= 1 (alist-ref (dbi-get_row connection) "count(type)")))
          table-names)))))