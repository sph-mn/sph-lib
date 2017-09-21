(library (sph filesystem stream)
  (export
    directory-delete-content
    directory-stream
    merge-files
    sph-filesystem-stream-description)
  (import
    (guile)
    (sph)
    (sph filesystem)
    (sph stream))

  (define sph-filesystem-stream-description
    "bindings that create or use srfi-41 streams for reading from directories")

  (define (directory-delete-content path)
    "string ->
     delete all files and directories under path.
     a bit safer than \"rm -r\" because it works only on directories"
    (let (path (ensure-trailing-slash path))
      (stream-each
        (l (a)
          (let (a (string-append path a))
            (if (directory? a) (begin (directory-delete-content a) (rmdir a)) (delete-file a))))
        (directory-stream path))))

  (define (directory-stream a) "directory-handle/string:path -> stream"
    (port->stream (if (string? a) (opendir a) a) readdir closedir))

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
