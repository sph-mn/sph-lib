(library (sph filesystem)
  (export
    call-with-directory
    directory-delete-content
    directory-list
    directory-reference?
    directory-stream
    directory-tree-paths
    dotfile?
    ensure-directory-structure
    ensure-trailing-slash
    file->string
    filename-extension
    find-file-any
    find-files-any
    fold-directory-tree
    get-unique-target-path
    is-directory?
    last
    merge-files
    mtime-difference
    path->full-path
    path->list
    path-append
    path-append*
    poll-watch
    remove-filename-extension
    search-load-path
    spath->path
    spath?
    stat-accessor->stat-field-name
    stat-diff
    stat-diff->accessors
    stat-field-name->stat-accessor
    string->file)
  (import
    (guile)
    (rnrs base)
    (rnrs bytevectors)
    (rnrs io ports)
    (sph)
    (sph read-write)
    (sph stream)
    (sph string)
    (sph time)
    (srfi srfi-41)
    (only (sph list)
      any->list
      length-greater-one?
      length-eq-one?)
    (only (sph one) call-at-approximated-interval)
    (only (sph string) string-replace-string)
    (only (srfi srfi-1)
      append-map
      delete!
      filter-map
      fold-right
      map!
      last))

  (define (directory-delete-content path)
    "string ->
    delete all files and directories under path.
    a bit safer than \"rm -r\" because it only works on directories"
    (let (path (ensure-trailing-slash path))
      (stream-each
        (l (e)
          (let (e (string-append path e))
            (if (is-directory? e) (begin (directory-delete-content e) (rmdir e)) (delete-file e))))
        (directory-stream path))))

  (define (is-directory? path) "test if path exists and is a directory"
    (eqv? (q directory) (stat:type (stat path))))

  (define (directory-reference? file-path)
    "string -> boolean
    test if given string designates a directory reference, either \".\" or \"..\"
    can be used as a filter to directory-listing procedures."
    (let ((name (basename file-path))) (if (or (string= name ".") (string= name "..")) #t #f)))

  (define (call-with-directory path proc) "string procedure:{directory-port -> any} -> any"
    (let* ((d (opendir path)) (r (proc d))) (closedir d) r))

  (define (directory-list path . include?)
    "path procedure:{filename -> boolean} ... -> (string ...)
    return a list of filename-entries in a directory, optionally filtered by one or multiple filter procedures"
    (let (include? (if (null? include?) (pair (negate directory-reference?) include?) include?))
      (call-with-directory path
        (l (d)
          (let loop ((e (readdir d)) (r (list)))
            (if (eof-object? e) r
              (loop (readdir d) (if (every (l (p) (p e)) include?) (pair e r) r))))))))

  (define (directory-stream directory . filter-proc)
    "string/directory-port [procedure ...] -> srfi-41-stream"
    (let
      ( (filter-proc (if (null? filter-proc) (list (negate directory-reference?)) filter-proc))
        (port (if (string? directory) (opendir directory) directory)))
      (stream-let loop ((e (readdir port)))
        (if (eof-object? e) (begin (closedir port) stream-null)
          (if (every (l (proc) (proc e)) filter-proc) (stream-cons e (loop (readdir port)))
            (loop (readdir port)))))))

  (define (directory-tree-paths path . filter-list)
    "-> (full-path ...)
    string procedure ... -> (string ...)
    results in a list of all paths under path, excluding path"
    (let*
      ( (dir (opendir path)) (path (ensure-trailing-slash path))
        (full-path (l (filename) (string-append path filename))))
      (let loop ((filename (readdir dir)) (directory-paths (list)) (other-paths (list)))
        (if (eof-object? filename)
          (if (null? directory-paths) other-paths
            (append other-paths (append-map directory-tree-paths directory-paths)))
          (if
            (or (string= "." filename) (string= ".." filename)
              (any (l (filter) (filter filename)) filter-list))
            (loop (readdir dir) directory-paths other-paths)
            (let ((cur-path (full-path filename)))
              (if (and (file-exists? cur-path) (equal? (q directory) (stat:type (stat cur-path))))
                (loop (readdir dir) (pair cur-path directory-paths) (pair cur-path other-paths))
                (loop (readdir dir) directory-paths (pair cur-path other-paths)))))))))

  (define (dotfile? name)
    "string -> boolean
    checks if name is non-empty and begins with a dot"
    (and (not (string-null? name)) (eqv? (string-ref name 0) #\.)))

  (define (ensure-directory-structure path)
    "string -> boolean
    try to create any directories of path that do not exist"
    (or (file-exists? path) (begin (ensure-directory-structure (dirname path)) (mkdir path))))

  (define (ensure-trailing-slash str) "string -> string"
    (if (or (string-null? str) (not (eqv? #\/ (string-ref str (- (string-length str) 1)))))
      (string-append str "/") str))

  (define (stat-field-name->stat-accessor a)
    "symbol -> guile-stat-accessor
    a guile-stat-accessor is for example stat:mtime, and the argument is as symbol for the part after stat:, in this case mtime.
    utility for functions working with file change events and stat-records"
    (if (eqv? (q mtime) a) stat:mtime
      (if (eqv? (q atime) a) stat:atime
        (if (eqv? (q size) a) stat:size
          (if (eqv? (q mode) a) stat:mode
            (if (eqv? (q uid) a) stat:uid
              (if (eqv? (q gid) a) stat:gid
                (if (eqv? (q nlink) a) stat:nlink (if (eqv? (q ctime) a) stat:ctime)))))))))

  (define (find-file-any relative-path search-path filter-proc)
    "-> (full-path . filename-extension)
    string\\symbol-path string (string ...) -> (string . string)
    find the first file corresponding to relative-path with a extension of fn-extensions."
    (let ((base-path (symbol-path->string relative-path search-path)))
      (let
        ( (construct-path (l (fn-extension) (string-append base-path "." fn-extension)))
          (filename (basename base-path)))
        (any
          (l (fn-extension)
            (let ((path (construct-path fn-extension)))
              (if (file-exists? path) (pair path fn-extension) #f)))
          filter-proc))))

  (define (file->string path\file)
    "string/file -> string
    open or use an opened file, read until end-of-file is reached and return a string of file contents"
    (if (string? path\file) (call-with-input-file path\file port->string) (port->string path\file)))

  (define (find-files-any search-path fn-extensions relative-paths)
    "string/(string/{relative-path -> search-path} ...) (string ...) symbol-path -> ((path . filename-extension) ...)
    supports multiple search-paths and procedures as search-path."
    (filter-map
      (if (list? search-path)
        (l (e)
          (any
            (l (search-path)
              (find-file-any e (if (procedure? search-path) (search-path e) search-path)
                fn-extensions))
            search-path))
        (l (e) (find-file-any e search-path fn-extensions)))
      relative-paths))

  (define* (fold-directory-tree proc init path #:key (max-depth (inf)) (before-filter (list)))
    "::
    procedure:{string:current-path guile-stat-object:stat-info any:previous-result -> any} -
    any string integer {string/path -> boolean} ...
    ->
    any:last-procedure-result

    fold over directory-tree under path, possibly limited by max-depth and with filenames filtered by \"before-filter.\""
    (let (path (ensure-trailing-slash path))
      (stream-fold
        (l (r e)
          (let* ((full-path (string-append path e)) (stat-info (stat full-path)))
            (if (and (eqv? (q directory) (stat:type stat-info)) (< 1 max-depth))
              (fold-directory-tree proc (proc full-path stat-info r)
                (string-append full-path "/") #:max-depth
                (- max-depth 1) #:before-filter before-filter)
              (proc full-path stat-info r))))
        init
        (directory-stream path
          (l (name) (and (not (directory-reference? name)) (before-filter name)))))))

  (define (filename-extension a)
    (let ((r (string-split a #\.))) (if (length-greater-one? r) (last r) #f)))

  (define (get-unique-target-path target-path)
    (if (file-exists? target-path)
      (let
        (new-target-path
          (string-append target-path "." (seconds->iso-date-string (stat:mtime (stat target-path)))))
        (if (file-exists? new-target-path)
          (let (fn-add-count (l (a count) (string-append a "." (number->string count 32))))
            (let loop ((t (fn-add-count new-target-path 2)) (loop-count 3))
              (if (file-exists? t)
                (loop (fn-add-count new-target-path loop-count) (+ 1 loop-count)) t)))
          new-target-path))
      target-path))

  (define (merge-files target-path . source-paths)
    (let ((buffer (make-string 2000000)))
      (call-with-output-file target-path
        (l (target-file)
          (each
            (l (source-path)
              (call-with-input-file source-path
                (l (source-file)
                  (stream-each (l (block) (put-bytevector target-file block))
                    (port->buffered-octet-stream source-file 200000)))))
            source-paths)))))

  (define (mtime-difference . paths)
    "string ... -> integer
    get the mtimes for paths and subtract from the first mtime all subsequent.
    at least one file has changed if the number is not zero"
    (apply - (par-map (compose stat:mtime stat) paths)))

  (define-syntax-rule (path-append-internal tail-map-proc first-arg args)
    (if (null? args) first-arg
      (string-join (pair (string-trim-right first-arg #\/) (map tail-map-proc args)) "/")))

  (define (path-append first-arg . args)
    "string ... -> string
    combine string representations of filesystem paths.
    the arguments don't need to have leading or trailing slashes.
    use this if redundant slashes in the middle of parts are irrelevant or
    don't occur, otherwise use path-append*.
    the complexity of joining strings to paths is easily underestimated."
    (path-append-internal (l (e) (string-trim-both e #\/)) first-arg args))

  (define (path-append* first-arg . args)
    "like path-append, but also removes redundant slashes in the middle of the parts to append."
    (path-append-internal
      (l (e) (string-join (delete! "" (string-split (string-trim-both e #\/) #\/)) "/")) first-arg
      args))

  (define (path->list path)
    "parse a string representation of a filesystem path to a list of its parts.
    an empty string as the first element in the list stands for the root directory"
    (let (lis (filter (l (e) (not (string-null? e))) (string-split path #\/)))
      (if (and path (not (string-null? path)) (eqv? (string-ref path 0) #\/)) (pair "" lis) lis)))

  (define (path->full-path path)
    "string -> string
    get the full filesystem path for a relative path or return a full path.
    also resolves the directory references . and .."
    (if (string-null? path) #f
      (string-join
        (let
          (path-list
            (path->list (if (eqv? #\/ (string-ref path 0)) path (string-append (getcwd) "/" path))))
          (reverse!
            (fold
              (l (e r) (if (string-equal? "." e) r (if (string-equal? ".." e) (tail r) (pair e r))))
              (list) path-list)))
        "/")))

  (define remove-filename-extension
    (let
      ( (remove-filename-extension-one
          (l (extension filename)
            (if (string-suffix? extension filename)
              (string-drop-right filename (+ 1 (string-length extension))) filename))))
      (lambda (name . fn-extensions)
        "string [string ...] -> string
        remove one or multiple filename-extensions from the end of a string."
        (if (null? fn-extensions)
          (let ((fn-extension (string-split name #\.)))
            (if (> (length fn-extension) 1)
              (remove-filename-extension-one (last fn-extension) name) name))
          (fold remove-filename-extension-one name fn-extensions)))))

  (define* (search-load-path path #:optional (load-paths %load-path))
    "searches for the first match of a relative-path in load-paths.
    searches from left to right in the load-paths.
    all paths in load-paths must end with a \"/\".
    searches in guiles %load-path by default"
    (any
      (l (base-path)
        (let (full-path (string-append base-path path)) (if (file-exists? full-path) full-path #f)))
      load-paths))

  (define (stat-accessor->stat-field-name a)
    "utility for functions working with file change events and stat-records"
    (if (eqv? (q mtime) a) stat:mtime
      (if (eqv? (q atime) a) stat:atime
        (if (eqv? (q size) a) stat:size
          (if (eqv? (q mode) a) stat:mode
            (if (eqv? (q uid) a) stat:uid
              (if (eqv? (q gid) a) stat:gid
                (if (eqv? (q nlink) a) stat:nlink (if (eqv? (q ctime) a) stat:ctime)))))))))

  (define (stat-diff->accessors stat-info-1 stat-info-2 accessors)
    "-> (stat-accessor ...)
    find the difference between two stat-records.
    filter accessors (stat:mtime for example) for fields which do not differ between two stat-records."
    (filter (l (e) (not (equal? (e stat-info-1) (e stat-info-2)))) accessors))

  (define (stat-diff stat-info-1 stat-info-2 accessors)
    "-> ((vector accessor field-value-1 field-value-2)/#f ...)
    find the difference between two stat-records.
    map accessors (stat:mtime for example) to vectors for fields which differ between two stat-records."
    (filter-map
      (l (e)
        (let ((value-1 (e stat-info-1)) (value-2 (e stat-info-2)))
          (if (equal? value-1 value-2) #f (vector e value-1 value-2))))
      accessors))

  (define (string->file a path) "write string into file at path, overwriting the file"
    (call-with-output-file path (l (file) (display a file))))

  (define* (symbol-path->string symbol-path #:optional base-path) "symbol/(symbol ...) -> string"
    (let
      ( (path
          (if (list? symbol-path) (string-join (map symbol->string symbol-path) "/")
            (if (symbol? symbol-path) (symbol->string symbol-path)
              (if (string? symbol-path) symbol-path #f)))))
      (if base-path (string-append (ensure-trailing-slash base-path) path) path)))

  (define (spath? a) (and (list? a) (not (null? a)) (symbol? (first a))))
  (define* (spath->path a) (string-join (map symbol->string a) "/"))

  (define* (poll-watch paths events proc min-interval #:optional (max-interval min-interval))
    "(string ...) (symbol ...) {diff file-descriptors stat-info ->} milliseconds [milliseconds] ->
    observe stat information of multiple files (which may be directories)
    by checking for events of change (which are the name of stat-record accessors, for example stat:mtime, without the stat: prefix)
    and call proc if any of those changes have occurred. the diff passed to proc is a result of stat-diff.
    the files are checked in intervals with sizes between min-interval and max-interval,
    automatically adjusting the interval size to match need."
    (let ((accessors (map stat-field-name->stat-accessor events)))
      (let
        ( (fdes (map (l (e) (open e O_RDONLY)) (any->list paths)))
          (max-interval (* max-interval 1000)) (min-interval (* min-interval 1000))
          (stat-diff* (l (si-1 si-2) (stat-diff->accessors si-1 si-2 accessors))))
        (call-at-approximated-interval
          (l (prev-stat-info)
            (let* ((stat-info (map stat fdes)) (diff (map stat-diff* prev-stat-info stat-info)))
              (if (every null? diff) (list -1 stat-info)
                (begin (proc diff fdes stat-info) (list 1 stat-info)))))
          min-interval max-interval 1.1 0.2 (map stat fdes))))))