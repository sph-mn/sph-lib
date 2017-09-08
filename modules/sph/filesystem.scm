(library (sph filesystem)
  (export
    call-with-directory
    directory-fold
    directory-list
    directory-list-full-path
    directory-read-all
    directory-reference?
    directory-tree-each
    directory-tree-paths
    dotfile?
    ensure-directory-structure
    ensure-directory-structure-and-new-mode
    ensure-trailing-slash
    filename-extension
    fold-directory-tree
    is-directory?
    last
    list->path
    make-path-unique
    mtime-difference
    path->full-path
    path->list
    path-append
    path-append*
    path-directories
    poll-watch
    readlink*
    realpath*
    remove-filename-extension
    remove-trailing-slash
    search-load-path
    stat-accessor->stat-field-name
    stat-diff
    stat-diff->accessors
    stat-field-name->stat-accessor)
  (import
    (guile)
    (ice-9 ftw)
    (ice-9 threads)
    (rnrs bytevectors)
    (rnrs io ports)
    (sph)
    (sph hashtable)
    (sph list)
    (sph one)
    (sph string)
    (except (srfi srfi-1) map))

  (define directory-read-all scandir)

  (define (is-directory? path) "test if path exists and is a directory"
    (eqv? (q directory) (stat:type (stat path))))

  (define (directory-fold path proc init)
    (let (d (if (string? path) (opendir path) path))
      (let loop ((e (readdir d)) (result init))
        (if (eof-object? e) (begin (closedir d) result) (loop (readdir d) (proc e result))))))

  (define (directory-reference? file-path)
    "string -> boolean
     test if given string designates a directory reference, either \".\" or \"..\"
     can be used as a filter to directory-listing procedures."
    (let ((name (basename file-path))) (if (or (string= name ".") (string= name "..")) #t #f)))

  (define (call-with-directory path proc) "string procedure:{directory-port -> any} -> any"
    (let (d (opendir path)) (begin-first (proc d) (closedir d))))

  (define (directory-list path . include?)
    "path procedure:{filename -> boolean} ... -> (string ...)
     return a list of filename-entries in a directory, optionally filtered by one or multiple filter procedures"
    (let (include? (if (null? include?) (pair (negate directory-reference?) include?) include?))
      (call-with-directory path
        (l (d)
          (let loop ((e (readdir d)) (r (list)))
            (if (eof-object? e) r
              (loop (readdir d) (if (every (l (p) (p e)) include?) (pair e r) r))))))))

  (define (directory-list-full-path path . filter-proc) "string procedure ... -> (string ...)"
    (let (path (ensure-trailing-slash path))
      (map (l (e) (path->full-path (string-append path e))) (apply directory-list path filter-proc))))

  (define* (directory-tree-paths path #:optional (select? (const #t)))
    "string [procedure:{any -> boolean}] -> (full-path ...)
     string procedure -> (string ...)
     results in a list of all paths under path, excluding path and the directory references \".\" and \"..\""
    ;breadth-first search
    (let (entries (or (directory-read-all path (negate directory-reference?)) (list)))
      (fold-right
        (l (a r)
          (let (a (string-append path "/" a))
            (let (stat-info (stat a))
              ( (if (eqv? (q directory) (stat:type stat-info))
                  (l (r) (append (directory-tree-paths a select?) r)) identity)
                (if (select? a stat-info) (pair a r) r)))))
        (list) entries)))

  (define (dotfile? name)
    "string -> boolean
     checks if name is non-empty and begins with a dot"
    (and (not (string-null? name)) (eqv? (string-ref name 0) #\.)))

  (define (ensure-directory-structure path)
    "string -> boolean
     try to create any directories of path that do not exist.
     every path part is considered a directory"
    (or (file-exists? path) (begin (ensure-directory-structure (dirname path)) (mkdir path))))

  (define (ensure-directory-structure-and-new-mode path mode)
    "string -> boolean
     like ensure-directory-structure but also sets the file mode/permissions for new directories.
     the mode is influenced by the umask"
    (or (file-exists? path)
      (begin (ensure-directory-structure-and-new-mode (dirname path) mode) (mkdir path mode))))

  (define (ensure-trailing-slash str) "string -> string"
    (if (or (string-null? str) (not (eqv? #\/ (string-ref str (- (string-length str) 1)))))
      (string-append str "/") str))

  (define (path-directories a)
    "string -> (string ...)
     creates a list of the full paths of all directories above the given path"
    (unfold (l (e) (or (string-equal? "/" e) (string-equal? "." e))) identity dirname a))

  (define stat-field-name->stat-accessor-ht
    (ht-create-symbol mtime stat:mtime
      atime stat:atime
      size stat:size mode stat:mode uid stat:uid gid stat:gid nlink stat:nlink ctime stat:ctime))

  (define (stat-field-name->stat-accessor a)
    "symbol -> guile-stat-accessor
     a guile-stat-accessor is for example stat:mtime, and the argument is as symbol for the part after stat:, in this case mtime.
     utility for functions working with file change events and stat-records"
    (ht-ref stat-field-name->stat-accessor-ht a))

  (define stat-accessor->stat-field-name-ht
    (ht-create stat:mtime (q mtime)
      stat:atime (q atime)
      stat:size (q size)
      stat:mode (q mode) stat:uid (q uid) stat:gid (q gid) stat:nlink (q nlink) stat:ctime (q ctime)))

  (define (stat-accessor->stat-field-name a)
    "utility for functions working with file change events and stat-records"
    (ht-ref stat-accessor->stat-field-name-ht a))

  (define* (fold-directory-tree proc init path #:optional (max-depth (inf)))
    "::
     procedure:{string:current-path guile-stat-object:stat-info any:previous-result -> any} any string [integer] {string/path -> boolean} ...
     ->
     any:last-procedure-result

     fold over directory-tree under path, possibly limited by max-depth.
     the directory-references \".\" and \"..\" are ignored.
     call to proc is (proc full-path stat-info previous-result/init)"
    (let (path (ensure-trailing-slash path))
      (fold
        (l (e r)
          (let* ((full-path (string-append path e)) (stat-info (stat full-path)))
            (if (and (eqv? (q directory) (stat:type stat-info)) (< 1 max-depth))
              (fold-directory-tree proc (proc full-path stat-info r)
                (string-append full-path "/") (- max-depth 1))
              (proc full-path stat-info r))))
        init (directory-list path (negate directory-reference?)))))

  (define* (directory-tree-each proc path #:optional (max-depth (inf)))
    "procedure:{string stat-object ->} string [integer] ->"
    (fold-directory-tree (l (path stat-info r) (proc path stat-info)) #f path max-depth))

  (define (filename-extension a)
    "string -> string
     results in the last dot-separated part of string or the empty-string if no such part exists"
    (let ((r (string-split a #\.))) (if (length-greater-one? r) (last r) "")))

  (define* (make-path-unique path #:optional suffix)
    "string [string] -> string
     if the given path with suffix already exists, insert a string between path and the suffix
     so that file name is unique in its directory. suffix is empty by default.
     eventually inserts a period and a base32 number.
     examples
       \"/tmp/abc\" -> \"/tmp/abc.1\"
       \"/tmp/abc\" \".scm\" -> \"/tmp/abc.1.scm\""
    (let* ((suffix (or suffix "")) (full-path (string-append path suffix)))
      (if (file-exists? full-path)
        (let (next (l (count) (string-append path "." (number->string count 32) suffix)))
          (let loop ((path (next 1)) (count 1))
            (if (file-exists? path) (loop (next count) (+ 1 count)) path)))
        full-path)))

  (define (mtime-difference . paths)
    "string ... -> integer
     get the mtimes for paths and subtract from the first mtime all subsequent.
     at least one file has changed if the number is not zero"
    (apply - (par-map (compose stat:mtime stat) paths)))

  (define (remove-trailing-slash a) "remove trailing slashes if existant, otherwise result in a"
    (string-trim-right a #\/))

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
    "string -> list
     parse a string representation of a filesystem path to a list of its parts.
     an empty string as the first element in the list stands for the root directory.
     removes unnecessary slashes"
    (if (string-null? path) (list)
      (let (result (remove string-null? (string-split path #\/)))
        (if (eqv? (string-ref path 0) #\/) (pair "" result) result))))

  (define* (list->path a)
    "(string ...) -> string
     for a full path prepend an empty string to the input. this is analogous to the output of path->list"
    (if (equal? (list "") a) "/" (string-join a "/")))

  (define (readlink* path)
    "string -> string
     like readlink but also resolves symlinks to symlinks until a non-symlink is found or a target does not exist"
    (let* ((path (readlink path)) (stat-info (false-if-exception (stat path))))
      (if stat-info (if (eq? (q symlink) (stat:type stat-info)) (readlink* path) path) path)))

  (define (realpath* path)
    "string -> false/string
     resolves the directory references \".\" and \"..\" as well as symlinks and removes unnecessary slashes.
     named realpath* because it does not use the posix realpath because guile currently does not include it.
     the foreign function interface could be an alternative"
    ;/.. = /
    (and-let* ((path (path->full-path path)) (path-list (path->list path)))
      (let loop ((rest (tail path-list)) (result (list "")))
        (if (null? rest) (list->path (reverse result))
          (let (a (first rest))
            (cond ((string-equal? "." a) (loop (tail rest) result))
              ((string-equal? ".." a) (loop (tail rest) (tail result)))
              (else
                (let*
                  ( (path (list->path (reverse (pair a result))))
                    (stat-info (false-if-exception (lstat path))))
                  (and stat-info
                    (if (eq? (q symlink) (stat:type stat-info))
                      (let (link-target (readlink* path))
                        (if (string-prefix? "/" link-target)
                          (loop (append (tail (path->list link-target)) (tail rest)) (list ""))
                          (loop (append (path->list link-target) (tail rest)) result)))
                      (loop (tail rest) (pair a result))))))))))))

  (define* (path->full-path path #:optional realpath?)
    "string -> string
     uses \"getcwd\" to complete relative paths.
     with \"getcwd\" the basename can be a symlink but all other parts have symlinks resolved.
     the environment variable PWD is not used because it is not automatically updated when the process changes directory"
    (if (string-prefix? "/" path) path (string-append (getcwd) "/" path)))

  (define remove-filename-extension
    (let
      ( (remove-filename-extension-one
          (l (extension filename)
            (if (string-suffix? extension filename)
              (string-drop-right filename (string-length extension)) filename))))
      (l* (name #:optional fn-extensions all?)
        "string [(string)] [boolean]-> string
        remove specific, all or the last filename-extension from a string.
        filename-extension: period characters-except-period ..."
        (if fn-extensions (fold remove-filename-extension-one name fn-extensions)
          (let (name-split (string-split name #\.))
            (if (null? (tail name-split)) name
              (if all? (first name-split)
                (string-drop-right name (+ 1 (string-length (last name-split)))))))))))

  (define* (search-load-path path #:optional (load-paths %load-path))
    "gives the first match of a relative-path in load-paths or false.
     all paths in load-paths must end with a \"/\".
     searches in guiles %load-path by default"
    (any
      (l (base-path)
        (let (full-path (string-append base-path path)) (if (file-exists? full-path) full-path #f)))
      load-paths))

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

  (define* (symbol-path->string symbol-path #:optional base-path) "symbol/(symbol ...) -> string"
    (let
      ( (path
          (if (list? symbol-path) (string-join (map symbol->string symbol-path) "/")
            (if (symbol? symbol-path) (symbol->string symbol-path)
              (if (string? symbol-path) symbol-path #f)))))
      (if base-path (string-append (ensure-trailing-slash base-path) path) path)))

  (define (symbol-path? a) (and (list? a) (not (null? a)) (symbol? (first a))))
  (define* (symbol-path->path a) (string-join (map symbol->string a) "/"))

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
