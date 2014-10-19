(library (sph filesystem versioning)
  (export
    versioning-create
    versioning-default-config
    versioning-restore)
  (import
    (guile)
    (sph base)
    (sph filesystem)
    (sph process)
    (sph read-write))

  ;depends on the "diff" and "patch" utilities. version-id is a monotonically increasing integer saved with hexadecimal representation

  (define-as versioning-default-config symbol-hashtable
    max-count 3 max-size (inf) path-versions "versions/" path-temp "temp/")

  (define (get-existing-version-ids path-versions) "string -> (integer ...)"
    (if (file-exists? path-versions)
      (map (l (e) (string->number e 16))
        (directory-list path-versions (l (e) (not (string-prefix? "." e)))))
      (list)))

  (define (get-current-version-id existing-version-ids) "(integer ...) -> integer"
    (+ 1 (apply max existing-version-ids)))

  (define (path-versions-string filename config) "string rnrs-hashtable -> string"
    (string-append (hashtable-ref config (q path-versions)) filename "/"))

  (define-syntax-rule (path-version-file-string path-versions version-id)
    ;"string integer -> string"
    (string-append path-versions (number->string version-id)))

  (define (path-restored-file-string filename version-id config)
    "string integer rnrs-hashtable -> string"
    (string-append (hashtable-ref config (q path-temp)) filename "." (number->string version-id)))

  (define (delete-oldest-versions after-version-id existing-version-ids path-versions)
    "integer (integer ...) string ->"
    (each
      (l (e)
        (if (< e after-version-id)
          (delete-file (string-append path-versions (number->string e 16)))))
      existing-version-ids))

  (define (unit-byte->bit a) (* a 8))
  (define (get-file-size path) (unit-byte->bit (stat:size (stat path))))

  (define (versioning-create type path proc config)
    "symbol:text/binary string procedure:{string ->} -> integer:current-version-id
    call \"proc\" with a path to an empty file which is the next version, moving the old file to
    the directory for previous versions set in \"config\""
    (if (> (get-file-size path) (hashtable-ref config (q max-size))) (proc path)
      (let*
        ( (path-versions (path-versions-string (basename path) config))
          (existing-version-ids (get-existing-version-ids path-versions))
          (version-id
            (if (null? existing-version-ids)
              (begin (ensure-directory-structure path-versions)
                (ensure-directory-structure (hashtable-ref config (q path-temp))) 1)
              (get-current-version-id existing-version-ids)))
          (path-version-file (path-version-file-string path-versions version-id)))
        (if (> (length existing-version-ids) (hashtable-ref config (q max-count)))
          (delete-oldest-versions (- version-id (hashtable-ref config (q max-count)))
            existing-version-ids path-versions))
        (copy-file path path-version-file) (proc path)
        (if (eqv? (q text) type)
          (string->file
            (execute-with-pipe port->string OPEN_READ "diff" "--unified" path path-version-file)
            path-version-file))
        version-id)))

  (define (versioning-restore type path version-id config)
    "symbol:text/binary string integer -> string
    result in a path to a file which is \"path\" at version \"version-id\".
    the result path is not necessarily a copy"
    (let* ((filename (basename path)) (path-versions (path-versions-string filename config)))
      (if (eqv? (q binary) type) (path-version-file-string path-versions version-id)
        (let
          ( (current-version-id (get-current-version-id (get-existing-version-ids path-versions)))
            (restored-path (path-restored-file-string filename version-id config)))
          (copy-file path restored-path)
          (let loop ((v (- current-version-id 1)))
            (if (>= v version-id)
              (begin
                (execute "patch" "--unified"
                  "--silent" "--reject-file=-"
                  restored-path (path-version-file-string path-versions v))
                (loop (- v 1)))))
          restored-path)))))