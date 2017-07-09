(library (sph filesystem stream)
  (export
    directory-delete-content
    directory-stream
    merge-files)
  (import
    (sph)
    (sph filesystem)
    (guile)
    (sph stream))

  (define (directory-delete-content path)
    "string ->
     delete all files and directories under path.
     a bit safer than \"rm -r\" because it only works on directories"
    (let (path (ensure-trailing-slash path))
      (stream-each
        (l (e)
          (let (e (string-append path e))
            (if (is-directory? e) (begin (directory-delete-content e) (rmdir e))
              (delete-file e))))
        (directory-stream path))))

  (define (directory-stream directory . filter-proc)
    "string/directory-port [procedure ...] -> srfi-41-stream"
    (let
      ( (filter-proc (if (null? filter-proc) (list (negate directory-reference?)) filter-proc))
        (port (if (string? directory) (opendir directory) directory)))
      (stream-let loop ((e (readdir port)))
        (if (eof-object? e) (begin (closedir port) stream-null)
          (if (every (l (proc) (proc e)) filter-proc) (stream-cons e (loop (readdir port)))
            (loop (readdir port)))))))

  (define (merge-files target-path . source-paths)
    "string string ... ->
     creates or truncates \"target-path\" and appends the contents of all source-paths in order"
    (call-with-output-file target-path
      (l (target-file)
        (each
          (l (source-path)
            (call-with-input-file source-path
              (l (source-file)
                (stream-each (l (block) (put-bytevector target-file block))
                  (port->buffered-octet-stream source-file 200000)))))
          source-paths)))))
