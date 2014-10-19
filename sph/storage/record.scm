(library (sph storage record)
  (export
    srs-count
    srs-create-element
    srs-create-layout
    srs-delete-collection
    srs-delete
    srs-delete-layout
    srs-ensure-element
    srs-exit
    srs-get-layout
    srs-get-record-layout
    srs-identify
    srs-init
    srs-layout?
    srs-layout-collection
    srs-layout-layout
    srs-layout-name
    srs-make-layout
    srs-make-layout-with-record
    srs-read
    srs-select
    srs-update-element
    srs-ide-field-name)
  (import
    (dbi dbi)
    (rnrs base)
    (only (srfi srfi-1) map! reverse! unfold)
    (only (guile) assoc-ref delete-file identity promise? simple-format current-error-port)
    (ice-9 receive)
    (ice-9 match)
    (only (ice-9 streams) make-stream stream->list)
    (sph)
    (only (sph alist) alist-ref alist-select)
    (only (sph string) any->string)
    (only (sph list) fold-multiple)
    (only (sph alist) list->group-alist)
    (sph record)
    (sph sql)
    (only (sph guile-dbi lib) dbi-last-query-ok?)
    (only (sph guile-dbi sql) dbi-sql-last-insert-rowid))

  ;persistent (sph record) storage using sqlite
  ;the module uses the word "collection" for what is typically called "database", and "layout" for "table"

  (define (parse-config-element ele table record-spec indexes)
    (match ele
      ((field-name datatype other-config ...
	  (or (and (quote index) index-name) ((quote index) index-name)))
	(list
	  (cons
	    (list (symbol->string field-name) (symbol->string field-name))
	    table)
	  (cons field-name record-spec)
	  (cons (cons index-name field-name) indexes)))
      ((field-name config-n ...)
	(list
	  (cons (map symbol->string (cons field-name config-n)) table)
	  (cons field-name record-spec)
	  indexes))
      (_ (throw (q syntax-error-config-element) ele))))

  (define (parse-field-spec field-spec) "field-spec -> sql-spec record-spec"
    (apply values
      (map reverse!
	(fold-multiple parse-config-element
	  field-spec
	  (list (string-append srs-ide-field-name " integer primary key"))
	  (list) (list)))))

  (define (create-indexes collection layout-name index-info)
    (each
      (l (ele)
	(let (field-names (map any->string (map tail (tail ele))))
	  (if (eq? (q index) (first ele))
	    (each (l (field-name)
		(dbi-query collection (sql-create-index (string-append "index_" field-name) layout-name field-name)))
	      field-names)
	    (dbi-query collection (sql-create-index (any->string (first ele)) layout-name field-names)))))
      (list->group-alist index-info first)))

  (define (dbi-query-w-retrieve connection query retrieve)
    "execute sql-query and result in an (ice-9 stream) of records or a list of values.
    dbi-connection string record -> stream/list"
    (dbi-query connection query)
    (if (dbi-last-query-ok? connection) (make-record-stream connection retrieve)
      (begin (simple-format (current-error-port) "~S\n" connection) #f)))

  (define (make-record-stream selection retrieve) "dbi-result record-layout -> ice-9-stream"
    (if selection (make-stream
	(if (symbol? retrieve)
	  (let (field-name (symbol->string retrieve))
	    (l (state) ((l (value) (if value (cons (assoc-ref value field-name) state) value)) (dbi-get_row state))))
	  ;the dbi-get_row procedure should ideally return a vector
	  (l (state) ((l (value) (if value (cons (alist->record value retrieve) state) value)) (dbi-get_row state))))
	selection)
      selection))

  (define srs-layout (make-record-layout (quote (collection name layout))))

  (define (record-layout->string-field-names rl)
    (map symbol->string (vector->list (record-field-names-unordered rl))))

  (define srs-ide-field-name "ide")

  (define (srs-count srs-layout)
    "count all stored records with the given layout"
    (let (collection (srs-layout-collection srs-layout))
      (dbi-query collection (sql-select (srs-layout-name srs-layout) "count()"))
      (tail (first (dbi-get_row collection)))))

  (define-record-accessors srs-layout
    (srs-layout-collection (q collection))
    (srs-layout-name (q name))
    (srs-layout-layout (q layout)))

  (define (srs-layout? arg)
    (and (record? arg) (srs-layout-collection arg) (srs-layout-name arg) (record-layout? (srs-layout-layout arg))))

  (define (srs-init path) "-> collection
    creates a connection handle to a record-storage, translating path and setting initial parameters" (dbi-open "sqlite3" path))

  (define (srs-exit collection) "layout/collection ->
    unregister connection-handle"
    (dbi-close (if (record? collection) (srs-layout-collection collection) collection)))

  (define (srs-delete-collection path) "string -> unspecified" (delete-file path))

  (define (srs-get-record-layout collection name)
    (let
      (record-spec
	(map string->symbol (stream->list
	    (dbi-query-w-retrieve collection (string-append "pragma table_info(" name ")") (q name)))))
      (if (null? record-spec) #f (make-record-layout record-spec))))

  (define (srs-get-layout collection name) "collection string -> layout
    get a srs-layout identified by name from an existing collection.
    the order may not be creation order"
    (let (record-layout (srs-get-record-layout collection name))
      (if record-layout (srs-make-layout-with-record collection name record-layout) #f)))

  (define (srs-create-layout collection name field-spec) "collection layout-name record-spec -> srs-layout
    srs-collection string record-spec -> srs-layout"
    (let-values (((table-info record-spec index-info) (parse-field-spec field-spec)))
      (dbi-query collection (sql-create-table name table-info))
      (if (not (null? index-info)) (create-indexes collection name index-info))
      (if (dbi-last-query-ok? collection)
	(srs-make-layout collection name record-spec)
	(throw (q fail-create-layout)))))

  (define (srs-delete-layout layout) "-> unspecified"
    (if (srs-layout? layout)
      (dbi-query (srs-layout-collection layout) (sql-delete-table (srs-layout-name layout)))
      (throw (q not-a-srs-layout))))

  (define (srs-create-element layout ele-data) "-> record-ide/success-status
    -> integer/boolean"
    (let (collection (srs-layout-collection layout))
      (if (dbi-query collection (sql-insert (srs-layout-name layout) ele-data))
	(dbi-sql-last-insert-rowid collection) #f)))

  (define* (srs-delete layout filter #:optional config)
    (dbi-query (srs-layout-collection layout)
      (sql-delete (srs-layout-name layout) filter config)))

  (define (srs-update-element layout ele-data ele-data-next)
    (dbi-query (srs-layout-collection layout) (sql-update (srs-layout-name layout) ele-data-next ele-data)))

  (define (srs-ensure-element layout ele-data)
    (let (ide (srs-identify layout (sql-ele-data->filter ele-data)))
      (if ide ide (srs-create-element layout ele-data))))

  (define* (srs-select layout #:optional filter retrieve config) "rld list rld alist -> stream
    prepare retrieval of records and return a stream of records"
    (if (srs-layout? layout)
      (call-with-values
        (l ()
          (if retrieve
	    (cond
	      ((symbol? retrieve) (values retrieve (symbol->string retrieve)))
              ((record-layout? retrieve) (values retrieve (record-layout->string-field-names retrieve)))
	      ((boolean? retrieve) (values (srs-layout-layout layout) (record-layout->string-field-names (srs-layout-layout layout))))
	      ((list? retrieve) (values (srs-layout-layout layout) (map symbol->string retrieve)))
	      ((string? retrieve) (values (string->symbol retrieve) retrieve))
	      (else (throw (q wrong-type-for-argument))))
	    (values (srs-layout-layout layout) (record-layout->string-field-names (srs-layout-layout layout)))))
        (l (retrieve-rld field-names)
          (dbi-query-w-retrieve (srs-layout-collection layout)
	    (sql-select (srs-layout-name layout) field-names filter config)
	    retrieve-rld)))
      (throw (q not-a-srs-layout))))

  (define (srs-read . args) "like srs-select, but returns records in a list"
    (let (res (apply srs-select args)) (if (promise? res) (stream->list res) res)))

  (define (srs-make-layout collection name record-spec) "srs-collection string srs-record-spec -> srs-layout
    create layout handle"
    (record srs-layout collection name (make-record-layout record-spec)))

  (define (srs-make-layout-with-record collection name record-layout)
    (record srs-layout collection name record-layout))

  (define (srs-identify layout filter)
    (let (res (srs-read layout filter srs-ide-field-name (list (cons (q limit) 1))))
      (if (null? res) #f (first res)))))