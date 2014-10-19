(library (sph guile-dbi sql)
  (export
    dbi-sql-read
    dbi-sql-count
    dbi-sql-last-insert-rowid
    dbi-sql-max
    dbi-sqlite-existing-tables?)
  (import
    (rnrs base)
    (ice-9 streams)
    (except (dbi dbi) dbi-query)
    (only (sph alist) alist-ref)
    (sph)
    (sph sql)
    (sph guile-dbi lib))


  (define (dbi-sql-last-insert-rowid connection) "-> integer
    supported by at least sqlite and mysql"
    (tail (first (dbi-get_row (dbi-query connection "select last_insert_rowid()")))))

  (define (dbi-sql-max connection table-name column-name)
    (let*
      ( (max-column (string-append "max(" column-name ")"))
	(max-value
          (alist-ref
            (first
              (stream->list
		(dbi-query
		  connection
		  (string-append "select " max-column " from " table-name))))
            max-column)))
      (if max-value max-value 0)))

  (define dbi-sqlite-existing-tables?
    (let
      ( (syn-query
          (l (name)
            (string-append "select count(type) from sqlite_master where type='table' and name='" name "' limit 1"))))
      (l (connection . table-names)
	"test if all tables exists in the database"
	(every
          (l (name)
            (dbi-query connection (syn-query name))
            (= 1 (alist-ref (dbi-get_row connection) "count(type)")))
          table-names)))))
